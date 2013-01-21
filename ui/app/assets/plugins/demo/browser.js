define(['css!plugins/code/code','text!plugins/code/browse2.html'], function(css, template){

	var ko = req('vendors/knockout-2.2.0'),
		key = req('vendors/keymage.min');

	var Browser = Class({
		title: ko.observable("Browse code"),
		tree: ko.observableArray(),
		path: ko.observableArray([]),
		init: function(parameters){
		},
		render: function(parameters){
			var view = $(template);
			this.update(parameters);
			ko.applyBindings(this, view[0]);

			// TODO : Refine key api
			key(parameters.url.replace("/","."), 'up', function(e){
				var target = $("dd.active, li.active", view)
					.prev()
					.getOrElse("dd:last, li:last", view)
					.addClass("active")

				target
					.siblings()
					.removeClass("active");

				// To autoscroll to link
				target.find("a")[0].focus();
			}, {preventDefault: true});
			key(parameters.url.replace("/","."), 'down', function(e){
				var target = $("dd.active, li.active", view)
					.next()
					.getOrElse("dd:first, li:first", view)
					.addClass("active")

				target
					.siblings()
					.removeClass("active");

				// To autoscroll to link
				target.find("a")[0].focus();
			}, {preventDefault: true});
			key(parameters.url.replace("/","."), 'left', function(e){
				var target = $("nav a", view).eq(-2)
						.getOrElse("nav a:first-child", view)
					window.location.hash = target.attr("href")
			});
			key(parameters.url.replace("/","."), 'right', function(e){
				var target = $("dd.active, li.active", view)
					.trigger("click");
			});

			return view;
		},
		update: function(parameters){
			var path = parameters.args.rest,
				url = path.length?"./"+path.join("/"):"./";
			this.load(url);
			this.breadcrumb(path);
		},
		load: function(url){
			var self = this;
			fetch(url)
				.done(function(datas){
					self.tree.removeAll();
					for (var i in datas.children){
						self.tree.push( datas.children[i] );
					}
				})
				.fail(function(){
					console.error("Render failed");
				});
		},
		breadcrumb: function(path){
			var self = this,
				root = "#demo/";
			this.path.removeAll();
			this.path.push({
				title: 'home',
				url: root
			});
			path.map(function(f){
				if (f){
					root += f +"/";
					self.path.push({
						'title': f,
						'url': root
					});
				}
			});
		},
		open: function(e){
			var target = e.location.replace("/Users/iamwarry/Work/Typesafe/Builder/snap/", "")
			window.location.hash = "demo/" + target;
			return false;
		}
	});

	// Fetch utility
	function fetch(url){
		return $.ajax({
			url: '/api/local/browse',
			type: 'GET',
			dataType: 'json',
			data: { location: url }
		});
	}

	return Browser;

});