--- tetgen.cxx_original	Tue Feb 26 21:35:33 2008
+++ tetgen.cxx	Tue Feb 26 13:49:41 2008
@@ -46,8 +46,32 @@
 }
 
 //
+// Delegator for tetrahedralize:
+//
+
+void delegate_tetrahedralize(int bs, tetgenbehavior *b, char *switches, 
+			     tetgenio *in, tetgenio *out, tetgenio *addin,
+			     tetgenio *bgmin)
+{
+  if(bs==0)
+    tetrahedralize(b, in, out, addin, bgmin);
+
+  if(bs==1)
+    tetrahedralize(switches, in, out, addin, bgmin);
+}
+
+//
 // Begin of class 'tetgenio' implementation
 //
+
+extern "C" 
+#ifdef WIN32
+__declspec(dllexport)
+#endif
+tetgenio* CreateObjectOfTetgenio()
+{
+  return new tetgenio();
+}
 
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
