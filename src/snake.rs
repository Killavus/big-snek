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

#[derive(Clone, PartialEq, Eq, Copy, Debug, Component)]
enum MoveDirection {
    Left,
    Right,
    Top,
    Bottom,
}

struct SnakeBodies(Vec<(Entity, Vec<Entity>)>);

const TILE_W: f32 = 16.0;
const TILE_H: f32 = 16.0;

impl GamePosition {
    fn new(x: i32, y: i32) -> Self {
        Self(IVec2::new(x, y))
    }

    fn in_window_space(&self) -> Vec3 {
        let self_f = self.as_vec2();
        let (x_f, y_f) = (self_f.x, self_f.y);

        Vec3::new((x_f * TILE_W).round(), (y_f * TILE_H).round(), 0.0)
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

const MAP_W: f32 = 60.0;
const MAP_H: f32 = 60.0;

fn setup(mut commands: Commands, mut windows: ResMut<Windows>, asset_server: Res<AssetServer>) {
    commands.spawn_bundle(Camera2dBundle::default());

    let snake_textures = SnakeTextures(
        asset_server.load("snake_head.png"),
        asset_server.load("snake_mid.png"),
        asset_server.load("snake_curve.png"),
        asset_server.load("snake_tail.png"),
    );

    let window = windows.get_primary_mut().unwrap();

    window.set_resizable(false);
    window.set_resolution(MAP_W * TILE_W, MAP_H * TILE_H);
    window.set_title("Big Snek".into());

    commands.insert_resource(MoveTimer(Timer::from_seconds(0.1, true)));
    commands.insert_resource(AppleTexture(asset_server.load("apple.png")));
    {
        use self::MoveDirection::*;
        let snakes = vec![spawn_snake(
            &GamePosition::new(0, 0),
            &[
                Left, Left, Bottom, Right, Right, Right, Right, Bottom, Left, Left, Left, Bottom,
                Bottom,
            ],
            &mut commands,
        )];

        commands.insert_resource(SnakeBodies(snakes));
    }

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
    mut timer: ResMut<MoveTimer>,
    snakes: Query<(Entity, &MoveDirection), With<Snake>>,
    bodies: Res<SnakeBodies>,
    mut segments: Query<&mut GamePosition, With<SnakeSegment>>,
) {
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

fn snake_controls(mut query: Query<&mut MoveDirection, With<Snake>>, keys: Res<Input<KeyCode>>) {
    if keys.just_pressed(KeyCode::W) {
        query.iter_mut().next().unwrap().turn(MoveDirection::Top);
    }

    if keys.just_pressed(KeyCode::S) {
        query.iter_mut().next().unwrap().turn(MoveDirection::Bottom);
    }

    if keys.just_pressed(KeyCode::A) {
        query.iter_mut().next().unwrap().turn(MoveDirection::Left);
    }

    if keys.just_pressed(KeyCode::D) {
        query.iter_mut().next().unwrap().turn(MoveDirection::Right);
    }
}

fn snake_draw(
    snakes: Query<Entity, With<Snake>>,
    mut segments: Query<(&GamePosition, &mut Handle<Image>, &mut Transform), With<SnakeSegment>>,
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
            let (position, mut texture, mut transform) = segments
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
        }
    }
}

struct MoveTimer(Timer);

impl Plugin for SnakeGame {
    fn build(&self, app: &mut App) {
        app.insert_resource(ClearColor(Color::rgb(1.0, 1.0, 1.0)))
            .add_startup_system(setup)
            .add_system(snake_move)
            .add_system(snake_draw)
            .add_system_to_stage(CoreStage::PostUpdate, snake_controls);
    }
}
