var Tuple0 = [] // Utils['Tuple0'];

if (typeof window === "undefined") {
    window = global;
}

if (typeof window.require === "undefined"){
    window.require = function(name){
        return window[name];
    };
};

var DefaultRollbar = {
    report: F3(function(r, l, m){
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback){
            return callback(_elm_lang$core$Native_Scheduler.succeed(Tuple0));
        });
    }),
    scopedRollbar: function(){
        return {
            ctor : 'Rollbar',
            _0: {}
        };
    },
    crash: F3(function(crash, _, message){
        console.error(message);
        return crash(message);
    })
};

var RollbarApi = function RollbarApi(Rollbar) {
    var scopedRollbar = function(){
        return function(filename){
            var options = {
                language: "Elm",
                context: filename,
            };

            var rollbar = Rollbar.scope(options);

            return {
                ctor : 'Rollbar',
                _0: rollbar
            };
        };
    };

    var report = function(Tuple0){
        return function(wrappedRollbar, level, message) {
            var rollbar = wrappedRollbar._0;

            return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback){
                if (!(level in rollbar)){
                    console.log("level ", level, " not found in rollbar ", rollbar);
                    return callback(_elm_lang$core$Native_Scheduler.succeed(Tuple0));
                }

                console.error(message);

                if (typeof window._elmEventSnapshots !== "undefined"){
                    rollbar[level](message, {snapshots : window._elmEventSnapshots});
                } else {
                    rollbar[level](message);
                }

                return callback(_elm_lang$core$Native_Scheduler.succeed(Tuple0));
            });
        };
    };

    var crash = function(){
        return function(crash, wrappedRollbar, message){
            console.error(message);
            wrappedRollbar._0.critical(message);
            return crash(message);
        };
    };

    return {
        scopedRollbar: scopedRollbar,
        report: report,
        crash: crash
    };
};

var _NoRedInk$noredink$Native_Rollbar = function (localRuntime) {
    var enableGlobalReporter = false;

    try {
        var Rollbar = require('Rollbar');
    } catch (e) {
        return DefaultRollbar;
    }

    var rollbar = RollbarApi(Rollbar);

    return {
        report: F3(rollbar.report(Tuple0)),
        scopedRollbar: rollbar.scopedRollbar(),
        crash: F3(rollbar.crash())
    };
}();
