package scalapsihost;

@SuppressWarnings({"unchecked", "rawtypes", "deprecation"})
public final class Host_JavaPsiFacade extends com.intellij.psi.JavaPsiFacade {
  private final Dispatch d;
  public Host_JavaPsiFacade(Dispatch d) { this.d = d; }
  @Override public com.intellij.psi.PsiClass findClass(java.lang.String a0, com.intellij.psi.search.GlobalSearchScope a1) { return (com.intellij.psi.PsiClass) d.call("JavaPsiFacade.findClass", new Object[]{a0, a1}); }
  @Override public com.intellij.psi.PsiJavaModule findModule(java.lang.String a0, com.intellij.psi.search.GlobalSearchScope a1) { return (com.intellij.psi.PsiJavaModule) d.call("JavaPsiFacade.findModule", new Object[]{a0, a1}); }
  @Override public com.intellij.psi.PsiElementFactory getElementFactory() { return (com.intellij.psi.PsiElementFactory) d.call("JavaPsiFacade.getElementFactory", new Object[]{}); }
  @Override public com.intellij.psi.PsiClass[] findClasses(java.lang.String a0, com.intellij.psi.search.GlobalSearchScope a1) { return (com.intellij.psi.PsiClass[]) d.call("JavaPsiFacade.findClasses", new Object[]{a0, a1}); }
  @Override public boolean hasClass(java.lang.String a0, com.intellij.psi.search.GlobalSearchScope a1) { return (Boolean) d.call("JavaPsiFacade.hasClass", new Object[]{a0, a1}); }
  @Override public com.intellij.psi.PsiPackage findPackage(java.lang.String a0) { return (com.intellij.psi.PsiPackage) d.call("JavaPsiFacade.findPackage", new Object[]{a0}); }
  @Override public java.util.Collection findModules(java.lang.String a0, com.intellij.psi.search.GlobalSearchScope a1) { return (java.util.Collection) d.call("JavaPsiFacade.findModules", new Object[]{a0, a1}); }
  @Override public com.intellij.psi.PsiJavaParserFacade getParserFacade() { return (com.intellij.psi.PsiJavaParserFacade) d.call("JavaPsiFacade.getParserFacade", new Object[]{}); }
  @Override public com.intellij.psi.PsiResolveHelper getResolveHelper() { return (com.intellij.psi.PsiResolveHelper) d.call("JavaPsiFacade.getResolveHelper", new Object[]{}); }
  @Override public com.intellij.psi.PsiNameHelper getNameHelper() { return (com.intellij.psi.PsiNameHelper) d.call("JavaPsiFacade.getNameHelper", new Object[]{}); }
  @Override public com.intellij.psi.PsiConstantEvaluationHelper getConstantEvaluationHelper() { return (com.intellij.psi.PsiConstantEvaluationHelper) d.call("JavaPsiFacade.getConstantEvaluationHelper", new Object[]{}); }
  @Override public boolean isPartOfPackagePrefix(java.lang.String a0) { return (Boolean) d.call("JavaPsiFacade.isPartOfPackagePrefix", new Object[]{a0}); }
  @Override public boolean isInPackage(com.intellij.psi.PsiElement a0, com.intellij.psi.PsiPackage a1) { return (Boolean) d.call("JavaPsiFacade.isInPackage", new Object[]{a0, a1}); }
  @Override public boolean arePackagesTheSame(com.intellij.psi.PsiElement a0, com.intellij.psi.PsiElement a1) { return (Boolean) d.call("JavaPsiFacade.arePackagesTheSame", new Object[]{a0, a1}); }
  @Override public boolean isConstantExpression(com.intellij.psi.PsiExpression a0) { return (Boolean) d.call("JavaPsiFacade.isConstantExpression", new Object[]{a0}); }
  @Override public com.intellij.openapi.project.Project getProject() { return (com.intellij.openapi.project.Project) d.call("JavaPsiFacade.getProject", new Object[]{}); }
}
