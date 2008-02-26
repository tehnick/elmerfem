--- tetgen.h_original	Tue Feb 26 21:35:39 2008
+++ tetgen.h	Tue Feb 26 13:48:16 2008
@@ -199,7 +199,7 @@
       int numberofvertices;
     } polygon;
 
-    static void init(polygon* p) {
+    virtual void init(polygon* p) {
       p->vertexlist = (int *) NULL;
       p->numberofvertices = 0;
     }
@@ -215,7 +215,7 @@
       int numberofholes;
     } facet;
 
-    static void init(facet* f) {
+    virtual void init(facet* f) {
       f->polygonlist = (polygon *) NULL;
       f->numberofpolygons = 0;
       f->holelist = (REAL *) NULL;
@@ -386,39 +386,39 @@
   public:
 
     // Initialize routine.
-    void initialize();
-    void deinitialize();
+    virtual void initialize();
+    virtual void deinitialize();
 
     // Input & output routines.
-    bool load_node_call(FILE* infile, int markers, char* nodefilename);
-    bool load_node(char* filename);
-    bool load_pbc(char* filename);
-    bool load_var(char* filename);
-    bool load_mtr(char* filename);
-    bool load_poly(char* filename);
-    bool load_off(char* filename);
-    bool load_ply(char* filename);
-    bool load_stl(char* filename);
-    bool load_medit(char* filename);
-    bool load_plc(char* filename, int object);
-    bool load_tetmesh(char* filename);
-    bool load_voronoi(char* filename);
-    void save_nodes(char* filename);
-    void save_elements(char* filename);
-    void save_faces(char* filename);
-    void save_edges(char* filename);
-    void save_neighbors(char* filename);
-    void save_poly(char* filename);
+    virtual bool load_node_call(FILE* infile, int markers, char* nodefilename);
+    virtual bool load_node(char* filename);
+    virtual bool load_pbc(char* filename);
+    virtual bool load_var(char* filename);
+    virtual bool load_mtr(char* filename);
+    virtual bool load_poly(char* filename);
+    virtual bool load_off(char* filename);
+    virtual bool load_ply(char* filename);
+    virtual bool load_stl(char* filename);
+    virtual bool load_medit(char* filename);
+    virtual bool load_plc(char* filename, int object);
+    virtual bool load_tetmesh(char* filename);
+    virtual bool load_voronoi(char* filename);
+    virtual void save_nodes(char* filename);
+    virtual void save_elements(char* filename);
+    virtual void save_faces(char* filename);
+    virtual void save_edges(char* filename);
+    virtual void save_neighbors(char* filename);
+    virtual void save_poly(char* filename);
 
     // Read line and parse string functions.
-    char *readline(char* string, FILE* infile, int *linenumber);
-    char *findnextfield(char* string);
-    char *readnumberline(char* string, FILE* infile, char* infilename);
-    char *findnextnumber(char* string);
+    virtual char *readline(char* string, FILE* infile, int *linenumber);
+    virtual char *findnextfield(char* string);
+    virtual char *readnumberline(char* string, FILE* infile, char* infilename);
+    virtual char *findnextnumber(char* string);
 
     // Constructor and destructor.
     tetgenio() {initialize();}
-    ~tetgenio() {deinitialize();}
+    virtual ~tetgenio() {deinitialize();}
 };
 
 ///////////////////////////////////////////////////////////////////////////////
@@ -523,15 +523,15 @@
     char bgmeshfilename[1024];
 
     tetgenbehavior();
-    ~tetgenbehavior() {}
+    virtual ~tetgenbehavior() {}
 
-    void versioninfo();
-    void syntax();
-    void usage();
+    virtual void versioninfo();
+    virtual void syntax();
+    virtual void usage();
 
     // Command line parse routine.
-    bool parse_commandline(int argc, char **argv);
-    bool parse_commandline(char *switches) {
+    virtual bool parse_commandline(int argc, char **argv);
+    virtual bool parse_commandline(char *switches) {
       return parse_commandline(0, &switches);
     }
 };
@@ -1912,5 +1912,12 @@
                     tetgenio *addin = NULL, tetgenio *bgmin = NULL);
 void tetrahedralize(char *switches, tetgenio *in, tetgenio *out,
                     tetgenio *addin = NULL, tetgenio *bgmin = NULL);
+
+extern "C"
+#ifdef WIN32
+__declspec(dllexport)
+#endif
+void delegate_tetrahedralize(int bs, tetgenbehavior *b, char *switches,
+  tetgenio *in, tetgenio *out, tetgenio *addin = NULL, tetgenio *bgmin = NULL);
 
 #endif // #ifndef tetgenH
