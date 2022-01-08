plfMA<-function(h,gtype,...){

	options(guiToolkit="tcltk")
	try(dispose(wp),silent=TRUE)
	plMA<-NULL;graph_type<-NULL;
	try({ if(!is.matrix(h) || is.data.frame(h)){ plMA<-as.matrix(h) } else if(is.matrix(h)){ plMA<-h }},silent=TRUE)
	if(!is.null(plMA))
	{
		na.gtype<-pmatch(gtype,c("scatter","hist","bar","box","pie","3d","ma","density","heatmap"))
		if(is.na(na.gtype))
			stop("invalid 'gtype' argument")
		graph_type<-gtype
	}
	wp<<-gwindow("plfMA",visible=FALSE,fill="x",width=1000,height=600)
	gp1<-ggroup(horizontal=FALSE,spacing=0,use.scrollwindow=FALSE,container=wp)
	gp2<-ggroup(horizontal=TRUE,container=gp1,spacing=3)
	dplot<-gbutton("Scatter-plot",container=gp2,anchor=c(-1,1));size(dplot)=10;
	dhist<-gbutton("Histo-gram",container=gp2,anchor=c(-1,1));size(dhist)=10;
	dbar<-gbutton("Bar-plot",container=gp2,anchor=c(-1,1));size(dbar)=10;
	dbox<-gbutton("Box-plot",container=gp2,anchor=c(-1,1));size(dbox)=10;
	dpie<-gbutton("Pie-chart",container=gp2,anchor=c(-1,1));size(dpie)=10;
	d3d<-gbutton("3D-plot",container=gp2,anchor=c(-1,1));size(d3d)=10;
	dma<-gbutton("MA-plot",container=gp2,anchor=c(-1,1));size(dma)=10;
	ddensity<-gbutton("Density-plot",container=gp2,anchor=c(-1,1));size(ddensity)=10;
	dheatmap<-gbutton("Heatmap",container=gp2,anchor=c(-1,1));size(dheatmap)=10;
	glabel("\t",container=gp2)
	import<-gbutton("Import",container=gp2,anchor=c(1,-1),spacing=50);size(import)=8;
	export<-gbutton("Export",container=gp2,anchor=c(1,-1));size(export)=8;
	
	gp3<-ggroup(horizontal=TRUE,container=gp1,expand=TRUE,spacing=3)
	gp4<-gframe(container=gp3,horizontal=FALSE,spacing=0)
	gp5<-gframe(container=gp3)
	
	gp401<-ggroup(container=gp4,spacing=2)
	glabel("Main text :",container=gp401,anchor=c(-1,1))
	main_text<-gedit("",initial.msg="",width=80,height=20,container=gp401,anchor=c(-1,1))
	size(main_text)<-40
	gp402<-ggroup(container=gp4,spacing=2)
	glabel("Sub title   :",container=gp402,anchor=c(-1,1))
	sub_text<-gedit("",initial.msg="",width=60,height=20,container=gp402,anchor=c(-1,1))
	size(sub_text)<-35
	gp403<-ggroup(container=gp4,spacing=2)
	glabel("X-label     :",container=gp403,anchor=c(-1,1))
	x_lab<-gedit("",initial.msg="",width=50,height=20,container=gp403,anchor=c(-1,1))
	size(x_lab)<-35
	gp404<-ggroup(container=gp4,spacing=2)
	glabel("Y-label     :",container=gp404,anchor=c(-1,1))
	y_lab<-gedit("",initial.msg="",width=50,height=20,container=gp404,anchor=c(-1,1))
	size(y_lab)<-35
	gp405<-ggroup(container=gp4,spacing=2)
	glabel("X-limits    :",container=gp405,anchor=c(-1,1))
	x_lim<-gedit("",initial.msg="s,e or a,b,c..",width=15,height=20,container=gp405,anchor=c(-1,1))
	size(x_lim)<-10
	glabel("   Y-limits    :",container=gp405,anchor=c(-1,1))
	y_lim<-gedit("",initial.msg="s,e or a,b,c..",width=15,height=20,container=gp405,anchor=c(-1,1))
	size(y_lim)<-10
	
	fc_family="serif";
	gp406<-ggroup(container=gp4,spacing=7)
	glabel("\t       Font Family",container=gp406,anchor=c(-1,1))
	families<-c("sans","serif","monospace")
	font_family<-gcombobox(families,selected=2,container=gp406,handler=function(hcf,...){
		x<-svalue(hcf$obj)
		fc_family<<-x
		}
	)
	size(font_family)=10
	gp407<-ggroup(container=gp4,spacing=2)
	glabel("\t Size\t      Color\t\t    Style",container=gp407,anchor=c(-1,1))
	f_size<-c("0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0","2.2","2.4")
	f_colors<-c("aliceblue","aquamarine","azure","beige","black","blue","brown","chocolate","coral","cornflowerblue",
	"cornsilk","cyan","darkblue","darkcyan","darkgray","darkgreen","darkgrey","darkkhaki","darkmagenta","darkolivegreen",
	"darkorange","darkorchid","darkred","darksalmon","darkseagreen","darkslateblue","darkslategray","darkslategrey",
	"darkturquoise","darkviolet","deeppink","deepskyblue","dimgray","dimgrey","dodgerblue","firebrick","floralwhite",
	"forestgreen","gainsboro","ghostwhite","gold","goldenrod","gray","green","greenyellow","honeydew","hotpink","indianred",
	"ivory","khaki","lavender","lavenderblush","lawngreen","lemonchiffon","lightblue","lightcoral","lightcyan","lightgoldenrod",
	"lightgoldenrodyellow","lightgray","lightgreen","lightgrey","lightpink","lightsalmon","lightseagreen","lightskyblue",
	"lightslateblue","lightslategray","lightslategrey","lightsteelblue","lightyellow","limegreen","linen","magenta",
	"maroon","mediumaquamarine","mediumblue","mediumorchid","mediumpurple","mediumseagreen","mediumslateblue",
	"mediumspringgreen","mediumturquoise","mediumvioletred","midnightblue","mintcream","mistyrose","moccasin","navajowhite",
	"navy","navyblue","oldlace","olivedrab","orange","orangered","orchid","palegoldenrod","palegreen","paleturquoise",
	"palevioletred","papayawhip","peachpuff","peru","pink","plum","powderblue","purple","red","rosybrown","royalblue",
	"saddlebrown","salmon","sandybrown","seagreen","seashell","sienna","skyblue","slateblue","slategray","slategrey",
	"snow",	"springgreen","steelblue","tan","thistle","tomato","turquoise","violet","violetred","wheat","white","whitesmoke",
	"yellow","yellowgreen")
	f_stylet<-c("Regular","Bold","Slanted","Bold & Slanted","Symbol")
	f_style<-c("Regular","Bold","Slanted","Bold & Slanted")
	fc_sizet=1;fc_colorst="chocolate";fc_stylet=1
	fc_sizes=1;fc_colorss="sienna";fc_styles=1
	fc_sizel=1;fc_colorsl="navy";fc_stylel=1
	fc_sizea=1;fc_colorsa="purple";fc_stylea=1
	gp408<-ggroup(container=gp4,spacing=2)
	glabel("Title      ",container=gp408,anchor=c(-1,1))
	font_sizet<-gcombobox(f_size,selected=5,container=gp408,handler=function(hcs,...){
		x<-svalue(hcs$obj)
		fc_sizet<<-x
		}
	)
	size(font_sizet)=8
	font_colorst<-gcombobox(f_colors,selected=8,container=gp408,handler=function(hcc,...){
		x<-svalue(hcc$obj)
		fc_colorst<<-x
		}
	)
	size(font_colorst)=12
	font_stylet<-gcombobox(f_stylet,selected=1,container=gp408,handler=function(hcsy,...){
		x<-svalue(hcsy$obj)
		if(x=="Regular")fc_stylet<<-1
		if(x=="Bold")fc_stylet<<-2
		if(x=="Slanted")fc_stylet<<-3
		if(x=="Bold & Slanted")fc_stylet<<-4
		if(x=="Symbol")fc_stylet<<-5
		}
	)
	size(font_stylet)=12
	gp409<-ggroup(container=gp4,spacing=2)
	glabel("Subtitle ",container=gp409,anchor=c(-1,1))
	font_sizes<-gcombobox(f_size,selected=5,container=gp409,handler=function(hcs,...){
		x<-svalue(hcs$obj)
		fc_sizes<<-x
		}
	)
	size(font_sizes)=8
	font_colorss<-gcombobox(f_colors,selected=116,container=gp409,handler=function(hcc,...){
		x<-svalue(hcc$obj)
		fc_colorss<<-x
		}
	)
	size(font_colorss)=12
	font_styles<-gcombobox(f_style,selected=1,container=gp409,handler=function(hcsy,...){
		x<-svalue(hcsy$obj)
		if(x=="Regular")fc_styles<<-1
		if(x=="Bold")fc_styles<<-2
		if(x=="Slanted")fc_styles<<-3
		if(x=="Bold & Slanted")fc_styles<<-4
		}
	)
	size(font_styles)=12
	gp410<-ggroup(container=gp4,spacing=2)
	glabel("Labels   ",container=gp410,anchor=c(-1,1))
	font_sizel<-gcombobox(f_size,selected=5,container=gp410,handler=function(hcs,...){
		x<-svalue(hcs$obj)
		fc_sizel<<-x
		}
	)
	size(font_sizel)=8
	font_colorsl<-gcombobox(f_colors,selected=90,container=gp410,handler=function(hcc,...){
		x<-svalue(hcc$obj)
		fc_colorsl<<-x
		}
	)
	size(font_colorsl)=12
	font_stylel<-gcombobox(f_style,selected=1,container=gp410,handler=function(hcsy,...){
		x<-svalue(hcsy$obj)
		if(x=="Regular")fc_stylel<<-1
		if(x=="Bold")fc_stylel<<-2
		if(x=="Slanted")fc_stylel<<-3
		if(x=="Bold & Slanted")fc_stylel<<-4
		}
	)
	size(font_stylel)=12
	gp411<-ggroup(container=gp4,spacing=2)
	glabel("Axis      ",container=gp411,anchor=c(-1,1))
	font_sizea<-gcombobox(f_size,selected=5,container=gp411,handler=function(hcs,...){
		x<-svalue(hcs$obj)
		fc_sizea<<-x
		}
	)
	size(font_sizea)=8
	font_colorsa<-gcombobox(f_colors,selected=107,container=gp411,handler=function(hcc,...){
		x<-svalue(hcc$obj)
		fc_colorsa<<-x
		}
	)
	size(font_colorsa)=12
	font_stylea<-gcombobox(f_style,selected=1,container=gp411,handler=function(hcsy,...){
		x<-svalue(hcsy$obj)
		if(x=="Regular")fc_stylea<<-1
		if(x=="Bold")fc_stylea<<-2
		if(x=="Slanted")fc_stylea<<-3
		if(x=="Bold & Slanted")fc_stylea<<-4
		}
	)
	size(font_stylea)=12
	gp412<-ggroup(container=gp4,spacing=3)
	glabel("        BG_color           FG color           Add               Border",container=gp412,anchor=c(-1,1))
	gp413<-ggroup(container=gp4,spacing=2)
	glabel("    ",container=gp413,anchor=c(-1,1))
	bgc_colors="wheat";fgc_colors="colorpalette";addc_colors="green";bdc_colors="maroon";
	bgc_s<-gcombobox(f_colors,selected=130,container=gp413,handler=function(hbgc,...){
		x<-svalue(hbgc$obj)
		bgc_colors<<-x
		}
	)
	size(bgc_s)<-10
	f_colors2<-c("colorpalette",f_colors)
	fgc_s<-gcombobox(f_colors2,selected=1,container=gp413,handler=function(hfgc,...){
		x<-svalue(hfgc$obj)
		fgc_colors<<-x
		if(fgc_colors=="colorpalette"){enabled(colors_hmap)<-TRUE;enabled(legend_hmap)<-TRUE}else{enabled(colors_hmap)<-FALSE;enabled(legend_hmap)<-FALSE}
		}
	)
	size(fgc_s)<-10
	addc_s<-gcombobox(f_colors,selected=44,container=gp413,handler=function(haddc,...){
		x<-svalue(haddc$obj)
		addc_colors<<-x
		}
	)
	size(addc_s)<-8
	bdc_s<-gcombobox(f_colors,selected=75,container=gp413,handler=function(hbdc,...){
		x<-svalue(hbdc$obj)
		bdc_colors<<-x
		}
	)
	size(bdc_s)<-8
	gp414<-ggroup(container=gp4,spacing=3)
	glabel("            Type\t\tLine_type         Line_width",container=gp414,anchor=c(-1,1))
	gp415<-ggroup(container=gp4,spacing=2)
	glabel("         ",container=gp415,anchor=c(-1,1))
	types<-c("points","lines","points & lines","lines point apart","lines points overplot","histogram like","stair steps","other steps","none")
	p_ty<-"p"
	plot_type<-gcombobox(types,selected=1,container=gp415,handler=function(htype,...){
		x<-svalue(htype$obj)
		if(x=="points"){p_ty<<-"p";enabled(pch_type)<-TRUE;enabled(lty_type)<-FALSE;enabled(lwd_type)<-TRUE}
		if(x=="lines"){p_ty<<-"l";enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="points & lines"){p_ty<<-"b";enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="lines point apart"){p_ty<<-"c";enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="lines points overplot"){p_ty<<-"o";enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="histogram like"){p_ty<<-"h";enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="stair steps"){p_ty<<-"s";enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="other steps"){p_ty<<-"S";enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE}
		if(x=="none"){p_ty<<-"n";enabled(pch_type)<-FALSE;enabled(lty_type)<-FALSE;enabled(lwd_type)<-FALSE}
		}
	)
	size(plot_type)=15
	lty_1_n<-c("blank","solid","dashed","dotted","dotdash","longdash","twodash")
	lty_s<-"solid"
	lty_type<-gcombobox(lty_1_n,selected=2,container=gp415,handler=function(hlty,...){
		x<-svalue(hlty$obj)
		lty_s<<-x
	}
	)
	size(lty_type)<-10
	lwd_type<-gedit("",initial.msg="",width=9,height=20,container=gp415,anchor=c(-1,1))
	size(lwd_type)<-10
	gp416<-ggroup(container=gp4,spacing=3)
	glabel("           Point_type\t Radius\t      Shade           Log",container=gp416,anchor=c(-1,1))
	gp417<-ggroup(container=gp4,spacing=2)
	glabel("        ",container=gp417,anchor=c(-1,1))
	pch_1_n<-c(1:25,32:127)
	pch_s<-1
	pch_type<-gcombobox(pch_1_n,selected=1,container=gp417,handler=function(hpch,...){
		x<-svalue(hpch$obj)
		pch_s<<-as.numeric(x)
	}	
	)
	size(pch_type)<-8

	types_r<-c(1,0.8,0.6,0.4,0.2,0,-0.2,-0.4,-0.6,-0.8,-1)
	ty_r<-1
	radius_type<-gcombobox(types_r,selected=1,container=gp417,handler=function(hrtype,...){
		x<-svalue(hrtype$obj)
		ty_r<<-as.numeric(x)
		}
	)
	size(radius_type)=8
	types_sh<-c(0,0.25,0.5,0.75,1)
	ty_sh<-0
	shade_type<-gcombobox(types_sh,selected=1,container=gp417,handler=function(hshtype,...){
		x<-svalue(hshtype$obj)
		ty_sh<<-x
		}
	)
	size(shade_type)=8
	types_log<-c("none","x","y","xy")
	ty_log<-""
	log_type<-gcombobox(types_log,selected=1,container=gp417,handler=function(hlogtype,...){
		x<-svalue(hlogtype$obj)
		if(x=="none"){ ty_log<<-"" } else { ty_log<<-x }
		}
	)
	size(log_type)=8
	gp418<-ggroup(container=gp4,spacing=3)
	glabel("            Dendrogram       Color-palette     Legend",container=gp418,anchor=c(-1,1))
	gp419<-ggroup(container=gp4,spacing=2)
	glabel("          ",container=gp419,anchor=c(-1,1))
	types_dend<-c("row","column","both","none")
	ty_dend<-"both"
	dend_type<-gcombobox(types_dend,selected=3,container=gp419,handler=function(hdendtype,...){
		x<-svalue(hdendtype$obj)
		ty_dend<<-x
	}	
	)
	size(dend_type)<-11
	hmap_colors<-c("Blues","Greens","Greys","Oranges","Purples","Reds",
	"BuPu","BuGn","GnBu","OrRd","PuBu","PuOr","PuRd","RdBu","RdGy","RdPu","YlGn",
	"BrBG","PiYG","PuBuGn","PRGn","RdYlBu","RdYlGn","YlGnBu","YlOrBr","YlOrRd","Spectral")
	c_hmap<-"Blues"
	colors_hmap<-gcombobox(hmap_colors,selected=1,container=gp419,handler=function(hhmapc,...){
		x<-svalue(hhmapc$obj)
		c_hmap<<-x
		}
	)
	size(colors_hmap)=10
	hmap_legend<-c("none","h_top","h_topright","h_topleft","h_bottom","h_bottomright","h_bottomleft","h_left","h_right","h_center",
	"v_top","v_topright","v_topleft","v_bottom","v_bottomright","v_bottomleft","v_left","v_right","v_center")
	l_hmap<-"none"
	legend_hmap<-gcombobox(hmap_legend,selected=1,container=gp419,handler=function(hhmapl,...){
		x<-svalue(hhmapl$obj)
		l_hmap<<-x
	}	
	)
	size(legend_hmap)<-12
	gp423<-ggroup(container=gp4,horizontal=FALSE,spacing=8)
	glabel("",container=gp423,anchor=c(-1,1))
	gp424<-ggroup(container=gp4,horizontal=FALSE,spacing=2)
	gp424_2<-ggroup(container=gp424,spacing=2)
	glabel("\t\t     ",container=gp424_2)
	set<-gbutton("Set",container=gp424_2,anchor=c(-1,1))
	size(set)=8
	reset<-gbutton("Reset",container=gp424_2,anchor=c(-1,1))
	size(set)=8
	exit<-gbutton("Exit",container=gp424_2,anchor=c(-1,1))
	size(exit)=8
	enabled(main_text)<-TRUE;enabled(sub_text)<-TRUE;enabled(x_lab)<-TRUE;enabled(y_lab)<-TRUE;enabled(font_family)<-TRUE;
	enabled(font_sizet)<-TRUE;enabled(font_colorst)<-TRUE;enabled(font_stylet)<-TRUE;
	enabled(font_sizes)<-TRUE;enabled(font_colorss)<-TRUE;enabled(font_styles)<-TRUE;
	enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
	enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
	enabled(bgc_s)<-TRUE;
	enabled(plot_type)<-FALSE;enabled(lty_type)<-FALSE;enabled(lwd_type)<-FALSE;
	enabled(x_lim)<-TRUE;enabled(y_lim)<-TRUE;
	enabled(fgc_s)<-FALSE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
	enabled(pch_type)<-FALSE;enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-FALSE;
	enabled(dend_type)<-FALSE;enabled(colors_hmap)<-TRUE;enabled(legend_hmap)<-TRUE;
	enabled(export)<-FALSE;
	hmap_colrbar="both-sides"
	click1=NULL;click2=NULL;click3=NULL;click4=NULL;click5=NULL;click6=NULL;click7=NULL;click8=NULL;click9=NULL;
	graph_type_run<-function(h,graph_type...){
		delete(gp3,gp5)
		if(!is.null(graph_type) && graph_type=="scatter")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plot(h)},hscale=1.38,vscale=1.13))
			click1<<-1;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
			enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-FALSE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="hist")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){hist(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-1;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-FALSE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="bar")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){barplot(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-1;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-TRUE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="box")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){boxplot(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-1;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-TRUE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-TRUE;enabled(shade_type)<-TRUE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="pie")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){pie(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-1;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-TRUE;enabled(shade_type)<-FALSE;enabled(log_type)<-FALSE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="3d")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){persp(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-1;click7<<-NULL;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-FALSE;enabled(font_colorsa)<-FALSE;enabled(font_stylea)<-FALSE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-TRUE;enabled(log_type)<-FALSE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="ma")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plotMA(h)},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-1;click8<<-NULL;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
			enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="density")
		{
			gp5<<-gframe(container=gp3)
			add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plot(density(h))},hscale=1.38,vscale=1.13))
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-1;click9<<-NULL;
			enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
			enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-FALSE;
			enabled(export)<-TRUE;
		} else 
		if(!is.null(graph_type) && graph_type=="heatmap")
		{
			hmap_w<-gwindow("Color bar",width=50,height=50,visible=FALSE)
			hmap_w1<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=hmap_w)
			hmap_colrbar_list<-gradio(c("row-side","column-side","both-sides","none"),selected=3,container=hmap_w1)
			hmap_w2<-ggroup(container=hmap_w1)
			addSpring(hmap_w1)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hncs,...){
				dispose(hmap_w)
				},container=hmap_w2,anchor=c(1,-1)
			)
			gbutton("OK",horizontal=TRUE,handler=function(hncs,...){
				hmap_colrbar<<-svalue(hmap_colrbar_list)
				dispose(hmap_w)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					delete(gp3,gp5)
					gp5<<-gframe(container=gp3)
					add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,RowSideColors=hmap_rc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					delete(gp3,gp5)
					gp5<<-gframe(container=gp3)
					add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,ColSideColors=hmap_cc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					delete(gp3,gp5)
					gp5<<-gframe(container=gp3)
					add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,RowSideColors=hmap_rc,ColSideColors=hmap_cc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
					} else {
					delete(gp3,gp5)
					gp5<<-gframe(container=gp3)
					add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
					}
				},container=hmap_w2,anchor=c(1,-1)
			)
			visible(wp)<-TRUE
			visible(hmap_w)<-TRUE
			click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-1;
			enabled(font_sizel)<-FALSE;enabled(font_colorsl)<-FALSE;enabled(font_stylel)<-FALSE;
			enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
			enabled(fgc_s)<-FALSE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
			enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
			enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
			enabled(dend_type)<-TRUE;enabled(colors_hmap)<-TRUE;enabled(legend_hmap)<-TRUE;
			enabled(export)<-TRUE;
		}
	}

	graph_type_run(h,graph_type)
	addHandlerClicked(dplot,handler=function(h2,...){
		graph_type<<-"scatter"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plot(h)},hscale=1.38,vscale=1.13))
		click1<<-1;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
		enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(dhist,handler=function(h2,...){
		graph_type<<-"hist"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){hist(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-1;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-FALSE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)	
	addHandlerClicked(dbar,handler=function(h2,...){
		graph_type<<-"bar"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){barplot(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-1;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-TRUE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(dbox,handler=function(h2,...){
		graph_type<<-"box"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){boxplot(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-1;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-TRUE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-TRUE;enabled(shade_type)<-TRUE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(dpie,handler=function(h2,...){
		graph_type<<-"pie"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){pie(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-1;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-TRUE;enabled(shade_type)<-FALSE;enabled(log_type)<-FALSE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(d3d,handler=function(h2,...){
		graph_type<<-"3d"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){persp(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-1;click7<<-NULL;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-FALSE;enabled(font_colorsa)<-FALSE;enabled(font_stylea)<-FALSE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-TRUE;enabled(log_type)<-FALSE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(dma,handler=function(h2,...){
		graph_type<<-"ma"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plotMA(h)},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-1;click8<<-NULL;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
		enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(ddensity,handler=function(h2,...){
		graph_type<<-"density"
		delete(gp3,gp5)
		gp5<<-gframe(container=gp3)
		add(gp5,tkrplot(getToolkitWidget(gp5),function(...){plot(density(h))},hscale=1.38,vscale=1.13))
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-1;click9<<-NULL;
		enabled(font_sizel)<-TRUE;enabled(font_colorsl)<-TRUE;enabled(font_stylel)<-TRUE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-TRUE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-FALSE;
		enabled(plot_type)<-TRUE;enabled(pch_type)<-TRUE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-FALSE;
		enabled(export)<-TRUE;
		}
	)
	addHandlerClicked(dheatmap,handler=function(h2,...){
		graph_type<<-"heatmap"
		hmap_w<-gwindow("Color bar",width=50,height=50)
		hmap_w1<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=hmap_w)
		hmap_colrbar_list<-gradio(c("row-side","column-side","both-sides","none"),selected=3,container=hmap_w1)
		hmap_w2<-ggroup(container=hmap_w1)
		addSpring(hmap_w1)
		gbutton("CANCEL",horizontal=TRUE,handler=function(hncs,...){
			dispose(hmap_w)
			},container=hmap_w2,anchor=c(1,-1)
		)
		gbutton("OK",horizontal=TRUE,handler=function(hncs,...){
			hmap_colrbar<<-svalue(hmap_colrbar_list)
			dispose(hmap_w)
			if(hmap_colrbar=="row-side"){
				hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
				delete(gp3,gp5)
				gp5<<-gframe(container=gp3)
				add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,RowSideColors=hmap_rc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
				} else if(hmap_colrbar=="column-side"){
				hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
				delete(gp3,gp5)
				gp5<<-gframe(container=gp3)
				add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,ColSideColors=hmap_cc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
				} else if(hmap_colrbar=="both-sides"){
				hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
				hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
				delete(gp3,gp5)
				gp5<<-gframe(container=gp3)
				add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,RowSideColors=hmap_rc,ColSideColors=hmap_cc,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
				} else {
				delete(gp3,gp5)
				gp5<<-gframe(container=gp3)
				add(gp5,tkrplot(getToolkitWidget(gp5),function(...){heatmap(h,col=colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)))},hscale=1.38,vscale=1.13))
				}
			},container=hmap_w2,anchor=c(1,-1)
		)
		click1<<-NULL;click2<<-NULL;click3<<-NULL;click4<<-NULL;click5<<-NULL;click6<<-NULL;click7<<-NULL;click8<<-NULL;click9<<-1;
		enabled(font_sizel)<-FALSE;enabled(font_colorsl)<-FALSE;enabled(font_stylel)<-FALSE;
		enabled(font_sizea)<-TRUE;enabled(font_colorsa)<-TRUE;enabled(font_stylea)<-TRUE;
		enabled(fgc_s)<-FALSE;enabled(addc_s)<-FALSE;enabled(bdc_s)<-TRUE;
		enabled(plot_type)<-FALSE;enabled(pch_type)<-FALSE;enabled(lty_type)<-TRUE;enabled(lwd_type)<-TRUE;
		enabled(radius_type)<-FALSE;enabled(shade_type)<-FALSE;enabled(log_type)<-TRUE;
		enabled(dend_type)<-TRUE;enabled(colors_hmap)<-TRUE;enabled(legend_hmap)<-TRUE;
		enabled(export)<-TRUE;
		}
	)

	addHandlerClicked(reset,handler=function(he1,...){
		dispose(wp)
		plfMA(plMA,graph_type)
		}
	)
	addHandlerClicked(exit,handler=function(he,...){
		dispose(wp)
		}
	)	
	addHandlerClicked(set,handler=function(h2,...){
		t1<-svalue(main_text)
		t2<-svalue(sub_text)
		t3<-svalue(x_lab)
		t4<-svalue(y_lab)
		t5=NULL;t6=NULL
		if(svalue(x_lim)!=""){
			rx<-svalue(x_lim)
			rx2<-strsplit(rx,",")
			rx3<-c(rx2[[1]][1],rx2[[1]][2])
			t5<-as.numeric(rx3)
			}
		if(svalue(y_lim)!=""){
			ry<-svalue(y_lim)
			ry2<-strsplit(ry,",")
			ry3<-c(ry2[[1]][1],ry2[[1]][2])
			t6<-as.numeric(ry3)
			}
		t7<-as.numeric(svalue(lwd_type))
		if(is.na(t7)==TRUE)t7=1
		if(graph_type=="scatter" || (click1==1 && length(c(click2,click3,click4,click5,click6,click7,click8,click9))==0))
		{
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						}
						l_hmap=="none"
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Scatter plot such data",title="Plotting Error")
			}
		} else	if(graph_type=="hist" || (click1==1 && length(c(click2,click3,click4,click5,click6,click7,click8,click9))==0) )
		{
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						}
						l_hmap=="none"
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Histogram such data",title="Plotting Error")
			}
		} else	if(graph_type=="bar" || (click3==1 && length(c(click1,click2,click4,click5,click6,click7,click8,click9))==0))
		{
			if(ty_log=="x" || ty_log=="y" || ty_log=="xy")enabled(addc_s)<-FALSE
			if(ty_log=="")enabled(addc_s)<-TRUE
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			f<-function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				par(family=fc_family)
				barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=fgc_colors,border=bdc_colors,
				axis.lty=lty_s,lwd=t7,
				log=ty_log)
				rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
				barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=fgc_colors,border=bdc_colors,
				axis.lty=lty_s,lwd=t7,add=TRUE,
				log=ty_log)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						}
						l_hmap=="none"
					}
			}
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),fun=f,hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Barplot such data",title="Plotting Error")
			}
		} else	if(graph_type=="box" || (click4==1 && length(c(click1,click2,click3,click5,click6,click7,click8,click9))==0))
		{
			if(ty_log=="x" || ty_log=="y" || ty_log=="xy")enabled(addc_s)<-FALSE
			if(ty_log=="")enabled(addc_s)<-TRUE
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			f<-function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
				boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,font.axis=fc_stylea,
				col.axis=bgc_colors,log=ty_log)
				rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
				boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
				family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,font.axis=fc_stylea,
				col.axis=fc_colorsa,log=ty_log,
				col=fgc_colors,bg=bgc_colors,border=bdc_colors,
				add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
				radius=ty_r,shade=ty_sh,type=p_ty)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						}
						l_hmap=="none"
					}
			}
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),fun=f,hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Boxplot such data",title="Plotting Error")
			}
		} else	if(graph_type=="pie" || (click5==1 && length(c(click1,click2,click3,click4,click6,click7,click8,click9))==0))
		{		
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
				pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						l_hmap=="none"
						}
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Pie chart such data",title="Plotting Error")
			}
		} else	if(graph_type=="3d" || (click6==1 && length(c(click1,click2,click3,click4,click5,click7,click8,click9))==0))
		{
			t5=NULL;t6=NULL;
			x=seq(0,1,length.out=nrow(h))
			y=seq(0,1,length.out=ncol(h))
			z=h
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col=fc_colorsl,font.lab=fc_stylel)
				persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
				family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						l_hmap=="none"
						}
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot 3D plot such data",title="Plotting Error")
			}
		} else	if(graph_type=="ma" || (click7==1 && length(c(click1,click2,click3,click4,click5,click6,click8,click9))==0))
		{
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						l_hmap=="none"
						}
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot MA plot such data",title="Plotting Error")
			}
		} else	if(graph_type=="density" || (click8==1 && length(c(click1,click2,click3,click4,click5,click6,click7,click9))==0))
		{
			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
				par(bg=bgc_colors)
				plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
				cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						l_hmap=="none"
						}
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot Density plot such data",title="Plotting Error")
			}
		} else	if(graph_type=="heatmap" || (click9==1 && length(c(click1,click2,click3,click4,click5,click6,click7,click8))==0))
		{

			img=NULL
			delete(gp3,gp5)
			gp5<<-gframe(container=gp3)
			img<-try({add(gp5,tkrplot(getToolkitWidget(gp5),function(...){
				fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
				if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
				if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
				if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
				{hmap_rowv=NULL;hmap_colv=NULL}
				t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
				if(length(t5)==0){t5<-NULL}else{t5<-t5}
				if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
				if(l_hmap=="none"){} else {
					legend_hv<-strsplit(l_hmap,"_")
					if(legend_hv[[1]][1]=="h"){
					legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						} else {
						legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
						l_hmap=="none"
						}
					}
				},hscale=1.38,vscale=1.13))},silent=TRUE)
			if(length(grep("Error in",img))!=0){
				gmessage("Cannot plot heatmap such data",title="Plotting Error")
			}
		}
	})

	newdata=NULL
	addHandlerClicked(import,handler=function(h5,...){
		newdata<-tclvalue(tkgetOpenFile())
		nd1<-read.table(newdata,header=TRUE)
		nd2<-as.matrix(nd1)
		if(dim(nd2)[1]!=0){
			h<<-nd2
			gtype_w<-gwindow("Graph type",width=150,height=200)
			gtype_w1<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=gtype_w)
			gtype_list<-gradio(c("Scatter-plot","Histo-gram","Bar-plot","Box-plot","Pie-chart","3D-plot","MA-plot","Density-plot","Heatmap"),container=gtype_w1)
			gtype_p<-svalue(gtype_list)
			gtype_w2<-ggroup(container=gtype_w1)
			addSpring(gtype_w1)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnd,...){
				dispose(gtype_w)
				},container=gtype_w2,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnd,...){
				gtype_p<-svalue(gtype_list)
				dispose(gtype_w)
				if(gtype_p=="Scatter-plot"){ graph_type<<-"scatter" } else if(gtype_p=="Histo-gram"){ graph_type<<-"hist" } else 
				if(gtype_p=="Bar-plot"){ graph_type<<-"bar" } else if(gtype_p=="Box-plot"){ graph_type<<-"box" } else 
				if(gtype_p=="Pie-chart"){ graph_type<<-"pie" } else if(gtype_p=="3D-plot"){ graph_type<<-"3d" } else 
				if(gtype_p=="MA-plot"){ graph_type<<-"ma" } else if(gtype_p=="Density-plot"){ graph_type<<-"density" } else 
				if(gtype_p=="Heatmap"){ graph_type<<-"heatmap" }
				graph_type_run(h,graph_type)
				},container=gtype_w2,anchor=c(1,-1))
		}	
	}
	)

	addHandlerClicked(export,handler=function(h3,...){
		t1<-svalue(main_text)
		t2<-svalue(sub_text)
		t3<-svalue(x_lab)
		t4<-svalue(y_lab)
		t5=NULL;t6=NULL
		if(svalue(x_lim)!="")
		{
			rx<-svalue(x_lim)
			rx2<-strsplit(rx,",")
			rx3<-c(rx2[[1]][1],rx2[[1]][2])
			t5<-as.numeric(rx3)
		}
		if(svalue(y_lim)!="")
		{
			ry<-svalue(y_lim)
			ry2<-strsplit(ry,",")
			ry3<-c(ry2[[1]][1],ry2[[1]][2])
			t6<-as.numeric(ry3)
		}
		t7<-as.numeric(svalue(lwd_type))
		if(is.na(t7)==TRUE)t7=1
		if(graph_type=="scatter" || (click1==1 && length(c(click2,click3,click4,click5,click6,click7,click8,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {
				
			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
 				} else if(length(grep(".png",jpegFileName))!=0)
				{
					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
 				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,col=fgc_colors,log=ty_log,pch=pch_s,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else 
		if(graph_type=="hist" || (click1==1 && length(c(click2,click3,click4,click5,click6,click7,click8,click9))==0))
		{
			t5=NULL
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
			    tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
 				} else if(length(grep(".png",jpegFileName))!=0)
				{
					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					hist(h,main=t1,sub=t2,xlab=t3,ylab=t4,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}
			
				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else 
		if(graph_type=="bar" || (click3==1 && length(c(click1,click2,click4,click5,click6,click7,click8,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
 				} else if(length(grep(".png",jpegFileName))!=0)
				{
					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,
					log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					barplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,
					axis.lty=lty_s,lwd=t7,add=TRUE,
					log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}    
				
				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else
		if(graph_type=="box" || (click4==1 && length(c(click1,click2,click3,click5,click6,click7,click8,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
 				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
 				} else if(length(grep(".png",jpegFileName))!=0)
 				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
 				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
 				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=bgc_colors,log=ty_log)
					rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=addc_colors)
					boxplot(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,
					family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,font.axis=fc_stylea,
					col.axis=fc_colorsa,log=ty_log,
					col=fgc_colors,bg=bgc_colors,border=bdc_colors,
					add=TRUE,pch=pch_s,lty=lty_s,lwd=t7,
					radius=ty_r,shade=ty_sh,type=p_ty)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else
		if(graph_type=="pie" || (click5==1 && length(c(click1,click2,click3,click4,click6,click7,click8,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
 				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
 					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
 				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
 					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						l_hmap=="none"
						}
					dev.off()
  				} else if(length(grep(".png",jpegFileName))!=0)
				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
 					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
 				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
 					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
				cex=fc_sizea,col=fc_colorsa,font=fc_stylea,lwd=t7)
					pie(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,border=bdc_colors,radius=ty_r,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else
		if(graph_type=="3d" || (click6==1 && length(c(click1,click2,click3,click4,click5,click7,click8,click9))==0))
		{
			t5=NULL;t6=NULL;
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {
			
			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
  				} else if(length(grep(".png",jpegFileName))!=0)
 				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
 				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					x=seq(0,1,length.out=nrow(h))
					y=seq(0,1,length.out=ncol(h))
					z=h
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					persp(x,y,z,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=range(x),ylim=range(y),zlim=range(z,na.rm=TRUE),
					family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					col=fgc_colors,border=bdc_colors,shade=ty_sh,lty=lty_s,lwd=t7)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else 
		if(graph_type=="ma" || (click7==1 && length(c(click1,click2,click3,click4,click5,click6,click8,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
  				} else if(length(grep(".png",jpegFileName))!=0)
 				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
 				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plotMA(h,main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					type=p_ty,bg.col=fgc_colors,bg.pch=as.numeric(pch_s),lty=lty_s,bg.cex=as.numeric(t7),log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else 
		if(graph_type=="density" || (click8==1 && length(c(click1,click2,click3,click4,click5,click6,click7,click9))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
  				} else if(length(grep(".png",jpegFileName))!=0)
 				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}									
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					if(fgc_colors=="colorpalette"){	fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) } else { fgc_colors<-fgc_colors }
					par(bg=bgc_colors)
					plot(density(h),main=t1,sub=t2,xlab=t3,ylab=t4,xlim=t5,ylim=t6,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cex.lab=fc_sizel,col.lab=fc_colorsl,font.lab=fc_stylel,
					cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,type=p_ty,pch=pch_s,lty=lty_s,lwd=t7,log=ty_log)
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else 
		
		
		if(graph_type=="heatmap" || (click9==1 && length(c(click1,click2,click3,click4,click5,click6,click7,click8))==0))
		{
			jpegFileName <- tclvalue(tkgetSaveFile(initialfile = "",filetypes = "{{BMP} {.bmp}} {{JPEG} {.jpeg}} {{PNG} {.png}} {{TIFF} {.tiff}} {{PDF} {.pdf}} {{POSTSCRIPT} {.ps}}"))
			if(!nchar(jpegFileName))
			{
				tkmessageBox(message=paste("The file was not saved",jpegFileName))
			} else {

			exp_w<-gwindow("File dimensions",width=100,height=100)
			exp_wg<-ggroup(horizontal=FALSE,use.scrollwindow=FALSE,container=exp_w)
			exp_wg0<-ggroup(container=exp_wg,horizontal=FALSE,spacing=2)
			glabel("BMP,JPEG,PNG,TIFF in 'Pixels'",container=exp_wg0,anchor=c(-1,1))
			glabel("PDF,PostScript in 'Inches'",container=exp_wg0,anchor=c(-1,1))			
			exp_wg1<-ggroup(container=exp_wg)
			glabel("Height            :",container=exp_wg1,anchor=c(-1,1))
			img_ht<-gedit("",initial.msg="",width=9,height=7,container=exp_wg1,anchor=c(-1,1))
			size(img_ht)<-8
			exp_wg2<-ggroup(container=exp_wg)
			glabel("Width             :",container=exp_wg2,anchor=c(-1,1))
			img_wd<-gedit("",initial.msg="",width=9,height=7,container=exp_wg2,anchor=c(-1,1))
			size(img_wd)<-8
			exp_wg3<-ggroup(container=exp_wg)
			glabel("Resolution     :",container=exp_wg3,anchor=c(-1,1))
			img_res<-gedit("",initial.msg="",width=9,height=7,container=exp_wg3,anchor=c(-1,1))
			size(img_res)<-8
			addSpring(exp_wg)
			exp_wg4<-ggroup(container=exp_wg)
			gbutton("CANCEL",horizontal=TRUE,handler=function(hnde,...){
				dispose(exp_w)
				},container=exp_wg4,anchor=c(1,-1))
			gbutton("OK",horizontal=TRUE,handler=function(hnde,...){
				img_ht_e<-as.numeric(svalue(img_ht))
				img_wd_e<-as.numeric(svalue(img_wd))
				img_res_e<-as.numeric(svalue(img_res))
				dispose(exp_wg4)

				if(length(grep(".bmp",jpegFileName))!=0)
				{
 					bmp(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".jpeg",jpegFileName))!=0)
				{
 					jpeg(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
  				} else if(length(grep(".png",jpegFileName))!=0)
 				{
 					png(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".tiff",jpegFileName))!=0)
				{
 					tiff(jpegFileName,height=img_ht_e,width=img_wd_e,res=img_res_e,units="px")
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
				} else if(length(grep(".pdf",jpegFileName))!=0)
				{
 					if(fc_family=="monospace"){fc_family<-"mono"}
					pdf(jpegFileName,height=img_ht_e,width=img_wd_e)
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				} else if(length(grep(".ps",jpegFileName))!=0)
 				{
					if(fc_family=="sans"){fc_family<-"Helvetica"} else
					if(fc_family=="serif"){fc_family<-"Times"} else
					if(fc_family=="monospace"){fc_family<-"Courier"} else {}
 					postscript(jpegFileName,height=img_ht_e,width=img_wd_e,family=fc_family)
					fgc_colors<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					if(ty_dend=="row"){hmap_rowv=NULL;hmap_colv=NA} else 
					if(ty_dend=="column"){hmap_rowv=NA;hmap_colv=NULL} else
					if(ty_dend=="none"){hmap_rowv=NA;hmap_colv=NA} else
					{hmap_rowv=NULL;hmap_colv=NULL}
					t5<-strsplit(svalue(x_lim),",")[[1]];t6<-strsplit(svalue(y_lim),",")[[1]]
					if(length(t5)==0){t5<-NULL}else{t5<-t5}
					if(length(t6)==0){t6<-NULL}else{t6<-t6}
				par(bg=bgc_colors)
				par(col=bdc_colors,lty=lty_s,lwd=t7)
				par(family=fc_family,cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
				cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
				cex.axis=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
				col=bdc_colors,lty=lty_s,lwd=t7)
				if(hmap_colrbar=="row-side"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h)) 
					heatmap(h,main=t1,sub=t2,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsl,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc)
					} else if(hmap_colrbar=="column-side"){
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h)) 
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					ColSideColors=hmap_cc)
					} else if(hmap_colrbar=="both-sides"){
					hmap_rc<-colorRampPalette(brewer.pal(8,c_hmap))(nrow(h))
					hmap_cc<-colorRampPalette(brewer.pal(8,c_hmap))(ncol(h))
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					labRow=t5,labCol=t6,
					family=fc_family,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE,
					RowSideColors=hmap_rc,ColSideColors=hmap_cc)
					} else {
					heatmap(h,main=t1,sub=t2,xlab=t3,ylab=t4,family=fc_family,
					cex.main=fc_sizet,col.main=fc_colorst,font.main=fc_stylet,
					cex.sub=fc_sizes,col.sub=fc_colorss,font.sub=fc_styles,
					labRow=t5,labCol=t6,
					family=fc_family,
					cexRow=fc_sizea,cexCol=fc_sizea,col.axis=fc_colorsa,font.axis=fc_stylea,
					col=fgc_colors,Rowv=hmap_rowv,Colv=hmap_colv,log=ty_log,add.expr=TRUE)
					}
					if(l_hmap=="none"){} else {
						legend_hv<-strsplit(l_hmap,"_")
						if(legend_hv[[1]][1]=="h"){
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),horiz=TRUE,border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							} else {
							legend(x=legend_hv[[1]][2],inset=0.01,legend=c("low","medium","high"),fill=colorRampPalette(brewer.pal(8,c_hmap))(3),border=NA,box.lwd=0.3,x.intersp = 0.2,cex=0.8)
							}
						l_hmap=="none"
						}
					dev.off()
    				}

				tkmessageBox(message=paste("The file was saved",jpegFileName))
				},container=exp_wg4,anchor=c(1,-1))
			}
		} else {}		
	})
	visible(wp)<-TRUE	

}
