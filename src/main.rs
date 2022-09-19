use bevy::prelude::*;

mod snake;

use snake::SnakeGame;

#[derive(Component)]
struct Person;

#[derive(Component)]
struct Name(String);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(SnakeGame)
        .run();
}
