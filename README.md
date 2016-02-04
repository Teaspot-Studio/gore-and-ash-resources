<<<<<<< HEAD
gore-and-ash-resources
==================

The module provides API for something for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/githubuser/gore-and-ash-resources.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `ResourcesT`:
``` haskell
type AppStack = ModuleStack [ResourcesT, ... other modules ... ] IO
```

And derive `MonadResources` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadResources)
```
=======
# gore-and-ash-resources
Resource module for gore-and-ash Engine
>>>>>>> 51ab889559c98a6ac885b48f4315500f1873f6c5
