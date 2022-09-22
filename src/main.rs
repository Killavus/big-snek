use bevy::prelude::*;

mod snake;

use snake::SnakeGame;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(SnakeGame)
        .run();
}
