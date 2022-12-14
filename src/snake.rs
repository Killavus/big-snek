use bevy::prelude::*;

pub struct SnakeGame;

#[derive(Component)]
struct Snake;

#[derive(Clone)]
struct SnakeTextures(Handle<Image>, Handle<Image>, Handle<Image>, Handle<Image>);

#[derive(Clone)]
struct AppleTexture(Handle<Image>);

#[derive(Component, Copy, Clone, Deref, DerefMut)]
struct GamePosition(IVec2);

#[derive(Component)]
struct SnakeSegment;

#[derive(Component)]
struct SnakeHead;

#[derive(Clone, PartialEq, Eq, Copy, Debug, Component)]
enum MoveDirection {
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Component)]
struct Apple;

struct SnakeBodies(Vec<(Entity, Vec<Entity>)>);

enum GameState {
    Starting { countdown: usize, timer: Timer },
    Paused { points: usize },
    Running { points: usize },
    GameOver { points: usize },
}

impl GameState {
    fn running(&self) -> bool {
        matches!(self, GameState::Running { .. })
    }
}

const TILE_W: f32 = 16.0;
const TILE_H: f32 = 16.0;

impl GamePosition {
    fn new(x: i32, y: i32) -> Self {
        Self(IVec2::new(x, y))
    }

    fn in_window_space(&self) -> Vec3 {
        let self_f = self.as_vec2();
        let (x_f, y_f) = (self_f.x, self_f.y);

        Vec3::new(
            (x_f * TILE_W).round(),
            (y_f * TILE_H).round() - (HEADER_H / 2.0),
            0.0,
        )
    }
}

fn spawn_snake(
    head_position: &GamePosition,
    segments_from_head: &[MoveDirection],
    commands: &mut Commands,
) -> (Entity, Vec<Entity>) {
    assert!(!segments_from_head.is_empty());
    let move_direction = segments_from_head[0].inverse();
    let snake_id = commands.spawn().insert(Snake).insert(move_direction).id();

    let mut last_position = *head_position;
    let mut segments = vec![commands
        .spawn_bundle(SpriteBundle { ..default() })
        .insert(*head_position)
        .insert(SnakeSegment)
        .insert(SnakeHead)
        .id()];

    for direction_from_last in segments_from_head {
        last_position.x += direction_from_last.x_offset();
        last_position.y += direction_from_last.y_offset();

        segments.push(
            commands
                .spawn_bundle(SpriteBundle { ..default() })
                .insert(last_position)
                .insert(SnakeSegment)
                .id(),
        );
    }

    (snake_id, segments)
}

#[derive(Component)]
struct UIScreenOverlay;

#[derive(Component)]
struct UICenterText;

#[derive(Component)]
struct UIRightHeaderText;

const MAP_W: f32 = 30.0;
const MAP_H: f32 = 30.0;
const HEADER_H: f32 = 32.0;

fn setup(
    mut commands: Commands,
    mut windows: ResMut<Windows>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut colors: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
) {
    commands.spawn_bundle(Camera2dBundle::default());

    let snake_textures = SnakeTextures(
        asset_server.load("snake_head.png"),
        asset_server.load("snake_mid.png"),
        asset_server.load("snake_curve.png"),
        asset_server.load("snake_tail.png"),
    );

    let window = windows.get_primary_mut().unwrap();

    window.set_resizable(false);
    window.set_resolution((MAP_W + 1.0) * TILE_W, (MAP_H + 1.0) * TILE_H + HEADER_H);
    window.set_title("Big Snek".into());

    commands
        .spawn_bundle(ColorMesh2dBundle {
            mesh: meshes
                .add(Mesh::from(shape::Quad::new(Vec2::new(
                    (MAP_W + 1.0) * TILE_W,
                    (MAP_H + 1.0) * TILE_H + HEADER_H,
                ))))
                .into(),
            material: colors.add(ColorMaterial {
                color: Color::rgba(0.0, 0.0, 0.0, 0.8),
                ..default()
            }),
            visibility: Visibility { is_visible: false },
            transform: Transform {
                translation: Vec3::new(0.0, 0.0, 0.02),
                ..default()
            },
            ..default()
        })
        .insert(UIScreenOverlay);

    let font = asset_server.load("pixeboy.ttf");

    commands.spawn_bundle(ColorMesh2dBundle {
        mesh: meshes
            .add(Mesh::from(shape::Quad::new(Vec2::new(
                (MAP_W + 1.0) * TILE_W,
                HEADER_H,
            ))))
            .into(),
        material: colors.add(ColorMaterial {
            color: Color::BLACK,
            ..default()
        }),
        transform: Transform {
            translation: Vec3::new(0.0, ((MAP_H + 1.0) * TILE_H) / 2.0, 0.03),
            ..default()
        },
        ..default()
    });

    commands
        .spawn_bundle(Text2dBundle {
            text: Text::from_section(
                String::from(""),
                TextStyle {
                    font: font.clone(),
                    font_size: 60.0,
                    color: Color::WHITE,
                },
            )
            .with_alignment(TextAlignment::CENTER),
            transform: Transform {
                translation: Vec3::new(0.0, -HEADER_H / 2.0, 0.04),
                ..default()
            },
            ..default()
        })
        .insert(UICenterText);

    commands
        .spawn_bundle(Text2dBundle {
            text: Text::from_section(
                String::from("POINTS: 0"),
                TextStyle {
                    font,
                    font_size: 14.0,
                    color: Color::WHITE,
                },
            )
            .with_alignment(TextAlignment::TOP_RIGHT),
            transform: Transform {
                translation: Vec3::new(
                    (MAP_W + 1.0) * TILE_W / 2.0 - 10.0,
                    (((MAP_H + 1.0) * TILE_H + HEADER_H) / 2.0) - 10.0,
                    0.04,
                ),
                ..default()
            },
            ..default()
        })
        .insert(UIRightHeaderText);

    commands.insert_resource(MoveTimer(Timer::from_seconds(0.1, true)));
    commands.insert_resource(AppleTexture(asset_server.load("apple.png")));
    {
        use self::MoveDirection::*;
        let snakes = vec![spawn_snake(
            &GamePosition::new(0, 0),
            &[Left],
            &mut commands,
        )];

        commands.insert_resource(SnakeBodies(snakes));
    }

    commands.insert_resource(GameState::Starting {
        countdown: 3,
        timer: Timer::from_seconds(1.0, true),
    });
    commands.insert_resource(snake_textures);
}

fn snake_ends_rotation(direction: &MoveDirection) -> Quat {
    use std::f32::consts::PI;
    use MoveDirection::*;

    match direction {
        Right => Quat::from_rotation_z(0.0),
        Left => Quat::from_rotation_y(PI),
        Top => Quat::from_rotation_z(PI / 2.0),
        Bottom => Quat::from_rotation_z(3.0 * PI / 2.0),
    }
}

/// It returns on which side target is relative to origin.
fn relative_direction(origin: &GamePosition, target: &GamePosition) -> MoveDirection {
    use std::cmp::Ordering::*;
    use MoveDirection::*;

    match (origin.x.cmp(&target.x), origin.y.cmp(&target.y)) {
        (Equal, Greater) => Bottom,
        (Equal, _) => Top,
        (Greater, Equal) => Left,
        (_, Equal) => Right,
        _ => panic!("diagonal relative directions are not supported"),
    }
}

fn is_curve(current_dir: &MoveDirection, next_dir: &MoveDirection) -> bool {
    use MoveDirection::*;

    match current_dir {
        Left | Right => next_dir == &Top || next_dir == &Bottom,
        MoveDirection::Top | MoveDirection::Bottom => next_dir == &Left || next_dir == &Right,
    }
}

fn snake_mid_rotation(direction: &MoveDirection) -> Quat {
    use self::MoveDirection::*;
    use std::f32::consts::PI;

    match direction {
        Left | Right => Quat::from_rotation_z(0.0),
        Bottom | Top => Quat::from_rotation_z(PI / 2.0),
    }
}

fn snake_curve_rotation(last_direction: &MoveDirection, next_direction: &MoveDirection) -> Quat {
    use self::MoveDirection::*;
    use std::f32::consts::PI;

    // Non-rotated is from Top to Left

    match (last_direction, next_direction) {
        (Left, Bottom) => Quat::from_rotation_z(PI),
        (Top, Right) => Quat::from_rotation_z(PI),
        (Top, Left) => Quat::from_rotation_x(PI),
        (Bottom, Right) => Quat::from_rotation_y(PI),
        (Right, Bottom) => Quat::from_rotation_x(PI),
        (Left, Top) => Quat::from_rotation_z(3.0 * PI / 2.0),
        _ => Quat::from_rotation_z(0.0),
    }
}

impl MoveDirection {
    fn x_offset(&self) -> i32 {
        use self::MoveDirection::*;

        match self {
            Left => -1,
            Right => 1,
            _ => 0,
        }
    }

    fn y_offset(&self) -> i32 {
        use self::MoveDirection::*;

        match self {
            Top => 1,
            Bottom => -1,
            _ => 0,
        }
    }

    fn is_horizontal(&self) -> bool {
        use self::MoveDirection::*;

        matches!(self, Left | Right)
    }

    fn is_vertical(&self) -> bool {
        !self.is_horizontal()
    }

    fn inverse(&self) -> MoveDirection {
        use self::MoveDirection::*;

        match self {
            Left => Right,
            Top => Bottom,
            Right => Left,
            Bottom => Top,
        }
    }

    fn turn(&mut self, new_direction: MoveDirection) {
        if new_direction.is_horizontal() && self.is_vertical()
            || self.is_horizontal() && new_direction.is_vertical()
        {
            *self = new_direction;
        }
    }
}

fn snake_move(
    time: Res<Time>,
    game_state: Res<GameState>,
    mut timer: ResMut<MoveTimer>,
    snakes: Query<(Entity, &MoveDirection), With<Snake>>,
    bodies: Res<SnakeBodies>,
    mut segments: Query<&mut GamePosition, With<SnakeSegment>>,
) {
    if !game_state.running() {
        return;
    }

    if timer.0.tick(time.delta()).just_finished() {
        for (entity, direction) in snakes.iter() {
            let body = &bodies
                .0
                .iter()
                .find(|(snake, _)| snake == &entity)
                .expect("snake body exists if entity is spawned")
                .1;

            let mut last_position: Option<GamePosition> = None;
            for segment in body.iter().copied() {
                let mut position = segments
                    .get_mut(segment)
                    .expect("segment entity exists if is a part of snake body");

                let saved_position = *position;
                match last_position {
                    None => {
                        position.x += direction.x_offset();
                        position.y += direction.y_offset();
                    }
                    Some(last_position) => {
                        position.x = last_position.x;
                        position.y = last_position.y;
                    }
                }

                last_position = Some(saved_position);
            }
        }
    }
}

fn snake_controls(
    mut query: Query<(Entity, &mut MoveDirection), With<Snake>>,
    bodies: Res<SnakeBodies>,
    segments: Query<&GamePosition, With<SnakeSegment>>,
    keys: Res<Input<KeyCode>>,
    mut game_state: ResMut<GameState>,
) {
    let (entity, mut move_direction) = query.single_mut();
    let body = &bodies
        .0
        .iter()
        .find(|(snake, _)| *snake == entity)
        .unwrap()
        .1;

    let mut last_direction = relative_direction(
        segments.get(body[1]).unwrap(),
        segments.get(body[0]).unwrap(),
    );

    if let GameState::Running { .. } = game_state.as_ref() {
        if keys.just_pressed(KeyCode::W) {
            *move_direction = {
                last_direction.turn(MoveDirection::Top);
                last_direction
            };
        }

        if keys.just_pressed(KeyCode::S) {
            *move_direction = {
                last_direction.turn(MoveDirection::Bottom);
                last_direction
            };
        }

        if keys.just_pressed(KeyCode::A) {
            *move_direction = {
                last_direction.turn(MoveDirection::Left);
                last_direction
            };
        }

        if keys.just_pressed(KeyCode::D) {
            *move_direction = {
                last_direction.turn(MoveDirection::Right);
                last_direction
            };
        }
    }

    if keys.just_pressed(KeyCode::P) {
        match *game_state {
            GameState::Paused { points } => {
                *game_state.as_mut() = GameState::Running { points };
            }
            GameState::Running { points } => {
                *game_state.as_mut() = GameState::Paused { points };
            }
            _ => {}
        }
    }
}

fn snake_draw(
    snakes: Query<Entity, With<Snake>>,
    mut segments: Query<
        (
            &GamePosition,
            &mut Handle<Image>,
            &mut Transform,
            Option<&SnakeHead>,
        ),
        With<SnakeSegment>,
    >,
    snake_bodies: Res<SnakeBodies>,
    snake_textures: Res<SnakeTextures>,
) {
    let SnakeTextures(head_tx, mid_tx, curve_tx, tail_tx) = snake_textures.as_ref();

    for entity in snakes.iter() {
        let body = &snake_bodies
            .0
            .iter()
            .find(|(snake, _)| snake == &entity)
            .expect("snake body exists if entity is spawned")
            .1;

        let mut body_iter = body.iter().copied().peekable();
        let mut last_direction_some: Option<MoveDirection> = None;

        while let Some(segment) = body_iter.next() {
            let prev_position = body_iter
                .peek()
                .and_then(|prev| segments.get(*prev).ok())
                .map(|result| *result.0);
            let (position, mut texture, mut transform, is_head) = segments
                .get_mut(segment)
                .expect("segment entity exists if part of snake body");

            match prev_position {
                None => {
                    if let Some(last_direction) = last_direction_some {
                        transform.rotation = snake_ends_rotation(&last_direction.inverse());
                        *texture.as_mut() = tail_tx.clone();
                    }
                }
                Some(prev_position) => {
                    let current_direction = relative_direction(position, &prev_position);

                    match last_direction_some {
                        Some(last_direction) => {
                            if is_curve(&last_direction, &current_direction) {
                                *texture.as_mut() = curve_tx.clone();
                                transform.rotation =
                                    snake_curve_rotation(&last_direction, &current_direction);
                            } else {
                                *texture.as_mut() = mid_tx.clone();
                                transform.rotation = snake_mid_rotation(&current_direction);
                            }
                        }
                        None => {
                            *texture.as_mut() = head_tx.clone();
                            transform.rotation = snake_ends_rotation(&current_direction.inverse());
                        }
                    }

                    last_direction_some = Some(current_direction);
                }
            }

            transform.translation = position.in_window_space();
            // In case of collision, draw the snake head on top.
            if is_head.is_some() {
                transform.translation.z = 0.01;
            }
        }
    }
}

fn snake_starting(mut game_state: ResMut<GameState>, time: Res<Time>) {
    let mut should_start = false;

    if let GameState::Starting { timer, countdown } = game_state.as_mut() {
        if timer.tick(time.delta()).just_finished() {
            *countdown = countdown.saturating_sub(1);
        }

        if *countdown == 0 {
            should_start = true;
        }
    }

    if should_start {
        *game_state.as_mut() = GameState::Running { points: 0 };
    }
}

fn state_draw(
    mut screen_overlay: Query<&mut Visibility, (With<UIScreenOverlay>, Without<UICenterText>)>,
    mut center_text: Query<(&mut Text, &mut Visibility), With<UICenterText>>,
    mut right_text: Query<&mut Text, (With<UIRightHeaderText>, Without<UICenterText>)>,
    game_state: Res<GameState>,
) {
    match game_state.as_ref() {
        GameState::Paused { .. } | GameState::GameOver { .. } | GameState::Starting { .. } => {
            screen_overlay.iter_mut().next().unwrap().is_visible = true;
        }
        _ => {
            screen_overlay.iter_mut().next().unwrap().is_visible = false;
        }
    }

    let (mut text, mut visibility) = center_text.single_mut();
    let mut center_text = text.sections.first_mut().unwrap();
    let mut text = right_text.single_mut();
    let mut right_text = text.sections.first_mut().unwrap();

    match game_state.as_ref() {
        GameState::Starting { countdown, .. } => {
            center_text.value = format!("{}", countdown);
            visibility.is_visible = true;
        }
        GameState::Paused { .. } => {
            center_text.value = "PAUSED".into();
            visibility.is_visible = true;
        }
        GameState::GameOver { .. } => {
            center_text.value = "GAME OVER".into();
            visibility.is_visible = true;
        }
        _ => {
            visibility.is_visible = false;
        }
    }

    match game_state.as_ref() {
        GameState::Paused { points }
        | GameState::GameOver { points }
        | GameState::Running { points } => {
            right_text.value = format!("POINTS: {}", points);
        }
        _ => {}
    }
}

fn snake_collision(
    mut game_state: ResMut<GameState>,
    snake_head: Query<&GamePosition, With<SnakeHead>>,
    other_segments: Query<&GamePosition, (Without<SnakeHead>, With<SnakeSegment>)>,
) {
    if let GameState::Running { points } = game_state.as_ref() {
        let head_pos = snake_head.single();
        let map_h = (MAP_H / 2.0).round() as i32;
        let map_w = (MAP_W / 2.0).round() as i32;

        let snake_collided = !(-map_w..=map_w).contains(&head_pos.x)
            || !(-map_h..=map_h).contains(&head_pos.y)
            || other_segments
                .iter()
                .any(|pos| pos.x == head_pos.x && pos.y == head_pos.y);

        if snake_collided {
            *game_state.as_mut() = GameState::GameOver { points: *points };
        }
    }
}

#[derive(Deref, DerefMut)]
struct AppleTimer(Timer);

impl Default for AppleTimer {
    fn default() -> Self {
        let mut timer = Timer::from_seconds(1.33, false);
        timer.pause();

        Self(timer)
    }
}

fn apple_spawner(
    mut commands: Commands,
    mut apple_timer: Local<AppleTimer>,
    game_state: Res<GameState>,
    time: Res<Time>,
    apple_texture: Res<AppleTexture>,
    used_positions: Query<&GamePosition, Without<Apple>>,
    apple: Query<(), With<Apple>>,
) {
    if !game_state.running() {
        return;
    }

    let first_spawn = if apple_timer.paused() {
        apple_timer.unpause();
        true
    } else {
        false
    };

    if first_spawn || apple.is_empty() && apple_timer.tick(time.delta()).just_finished() {
        use itertools::Itertools;
        use rand::prelude::*;
        use rand::seq::IteratorRandom;
        use std::collections::HashSet;

        let mut rng = thread_rng();

        let used_positions: HashSet<(i32, i32)> = used_positions
            .iter()
            .copied()
            .map(|pos| (pos.x, pos.y))
            .collect();

        let map_w = (MAP_W / 2.0).round() as i32;
        let map_h = (MAP_H / 2.0).round() as i32;

        let pos = (-map_w..=map_w)
            .cartesian_product(-map_h..=map_h)
            .into_iter()
            .filter(|(x, y)| !used_positions.contains(&(*x, *y)))
            .choose(&mut rng);

        if let Some((apple_x, apple_y)) = pos {
            commands
                .spawn_bundle(SpriteBundle {
                    texture: apple_texture.0.clone(),
                    visibility: Visibility { is_visible: false },
                    ..default()
                })
                .insert(GamePosition::new(apple_x, apple_y))
                .insert(Apple);
        }
    } else if apple.is_empty() && apple_timer.finished() {
        apple_timer.reset();
    }
}

fn apple_draw(mut apple: Query<(&GamePosition, &mut Transform, &mut Visibility), With<Apple>>) {
    if !apple.is_empty() {
        let (game_pos, mut transform, mut visibility) = apple.single_mut();
        transform.translation = game_pos.in_window_space();
        visibility.is_visible = true;
    }
}

fn apple_collision(
    game_state: Res<GameState>,
    mut commands: Commands,
    mut apple: Query<(Entity, &GamePosition), With<Apple>>,
    snake_head: Query<&GamePosition, With<SnakeHead>>,
    mut event: EventWriter<AppleEaten>,
) {
    if !game_state.running() {
        return;
    }

    if apple.is_empty() {
        return;
    }

    let head_pos = snake_head.single();
    let (apple_id, apple_pos) = apple.single_mut();

    if apple_pos.x == head_pos.x && apple_pos.y == head_pos.y {
        commands.entity(apple_id).despawn();
        event.send(AppleEaten);
    }
}

fn consume_apple(
    mut commands: Commands,
    mut ev_apple: EventReader<AppleEaten>,
    mut game_state: ResMut<GameState>,
    mut snake_bodies: ResMut<SnakeBodies>,
    snakes: Query<Entity, With<Snake>>,
    segments: Query<&GamePosition, With<SnakeSegment>>,
) {
    for _ in ev_apple.iter() {
        if let GameState::Running { points } = game_state.as_mut() {
            *points += 1;
        }

        for entity in snakes.iter() {
            let snake_body = &mut snake_bodies
                .0
                .iter_mut()
                .find(|(snake, _)| snake == &entity)
                .expect("snake body exists if entity is spawned")
                .1;

            let (tail_pos, direction) =
                snake_body
                    .iter()
                    .fold((None, None), |(last_pos, _), segment| {
                        let segment_pos =
                            segments.get(*segment).expect("segment exists if in body");

                        if let Some(last_pos) = last_pos {
                            let direction = relative_direction(&last_pos, segment_pos);
                            (Some(*segment_pos), Some(direction))
                        } else {
                            (Some(*segment_pos), None)
                        }
                    });

            if let Some((mut tail_pos, direction)) = tail_pos.zip(direction) {
                snake_body.push(
                    commands
                        .spawn_bundle(SpriteBundle { ..default() })
                        .insert({
                            tail_pos.x += direction.x_offset();
                            tail_pos.y += direction.y_offset();
                            tail_pos
                        })
                        .insert(SnakeSegment)
                        .id(),
                );
            }
        }
    }
}

struct MoveTimer(Timer);

struct AppleEaten;

impl Plugin for SnakeGame {
    fn build(&self, app: &mut App) {
        app.insert_resource(ClearColor(Color::BEIGE))
            .add_event::<AppleEaten>()
            .add_startup_system(setup)
            .add_system(snake_starting)
            .add_system(snake_move)
            .add_system(snake_collision)
            .add_system(snake_draw)
            .add_system(state_draw)
            .add_system(apple_draw)
            .add_system(apple_spawner)
            .add_system(apple_collision)
            .add_system(consume_apple)
            .add_system_to_stage(CoreStage::PostUpdate, snake_controls);
    }
}
