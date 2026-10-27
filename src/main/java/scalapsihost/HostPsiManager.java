package scalapsihost;

import com.intellij.codeInsight.multiverse.CodeInsightContext;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileFilter;
import com.intellij.psi.*;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.impl.PsiTreeChangeEventImpl;
import com.intellij.psi.impl.PsiTreeChangePreprocessor;
import com.intellij.psi.impl.file.impl.FileManager;
import com.intellij.psi.impl.file.impl.FileManagerEx;
import com.intellij.psi.util.PsiModificationTracker;

/**
 * Host-owned PsiManager. Every method records its call; methods the host
 * has not consciously supported throw, so unexpected integration shows up as an error.
 */
public final class HostPsiManager extends PsiManagerEx {
  private final Project project;
  private final PsiModificationTracker tracker;
  private final FileManager fileManager;

  public HostPsiManager(Project project, PsiModificationTracker tracker, FileManager fileManager) {
    this.project = project;
    this.tracker = tracker;
    this.fileManager = fileManager;
  }

  private static <T> T no(String name) { return CallLog.unsupported("PsiManager." + name); }
  private static void hit(String name) { CallLog.hit("PsiManager." + name); }

  // Supported: identity, lifecycle and tree-change notifications (no listeners exist in the host).
  @Override public Project getProject() { hit("getProject"); return project; }
  @Override public boolean isDisposed() { hit("isDisposed"); return false; }
  @Override public boolean areElementsEquivalent(PsiElement a, PsiElement b) { hit("areElementsEquivalent"); return a == b; }
  @Override public PsiModificationTracker getModificationTracker() { hit("getModificationTracker"); return tracker; }
  @Override public boolean isBatchFilesProcessingMode() { hit("isBatchFilesProcessingMode"); return false; }
  @Override public void beforeChildAddition(PsiTreeChangeEventImpl e) { hit("beforeChildAddition"); }
  @Override public void beforeChildRemoval(PsiTreeChangeEventImpl e) { hit("beforeChildRemoval"); }
  @Override public void beforeChildReplacement(PsiTreeChangeEventImpl e) { hit("beforeChildReplacement"); }
  @Override public void beforeChildrenChange(PsiTreeChangeEventImpl e) { hit("beforeChildrenChange"); }
  @Override public void beforeChildMovement(PsiTreeChangeEventImpl e) { hit("beforeChildMovement"); }
  @Override public void beforePropertyChange(PsiTreeChangeEventImpl e) { hit("beforePropertyChange"); }
  @Override public void childAdded(PsiTreeChangeEventImpl e) { hit("childAdded"); }
  @Override public void childRemoved(PsiTreeChangeEventImpl e) { hit("childRemoved"); }
  @Override public void childReplaced(PsiTreeChangeEventImpl e) { hit("childReplaced"); }
  @Override public void childMoved(PsiTreeChangeEventImpl e) { hit("childMoved"); }
  @Override public void childrenChanged(PsiTreeChangeEventImpl e) { hit("childrenChanged"); }
  @Override public void propertyChanged(PsiTreeChangeEventImpl e) { hit("propertyChanged"); }
  @Override public void beforeChange(boolean physical) { hit("beforeChange"); }
  @Override public void afterChange(boolean physical) { hit("afterChange"); }

  // Unsupported until a scenario proves they are needed.
  @Override public void cleanupForNextTest() { no("cleanupForNextTest"); }
  @Override public void dropResolveCacheRegularly(ProgressIndicator i) { no("dropResolveCacheRegularly"); }
  @Override public void setAssertOnFileLoadingFilter(VirtualFileFilter f, Disposable d) { no("setAssertOnFileLoadingFilter"); }
  @Override public boolean isAssertOnFileLoading(VirtualFile f) { return no("isAssertOnFileLoading"); }
  @Override public FileManager getFileManager() { hit("getFileManager"); return fileManager; }
  @Override public FileManagerEx getFileManagerEx() { return no("getFileManagerEx"); }
  @Override public void addTreeChangePreprocessor(PsiTreeChangePreprocessor p, Disposable d) { no("addTreeChangePreprocessor"); }
  @Override public void addTreeChangePreprocessor(PsiTreeChangePreprocessor p) { no("addTreeChangePreprocessor"); }
  @Override public void removeTreeChangePreprocessor(PsiTreeChangePreprocessor p) { no("removeTreeChangePreprocessor"); }
  @Override public void addTreeChangePreprocessorBackgroundable(PsiTreeChangePreprocessor p, Disposable d) { no("addTreeChangePreprocessorBackgroundable"); }
  @Override public PsiFile findFile(VirtualFile f) { return no("findFile"); }
  @Override public PsiFile findFile(VirtualFile f, CodeInsightContext c) { return no("findFile"); }
  @Override public FileViewProvider findViewProvider(VirtualFile f) { return no("findViewProvider"); }
  @Override public FileViewProvider findViewProvider(VirtualFile f, CodeInsightContext c) { return no("findViewProvider"); }
  @Override public PsiDirectory findDirectory(VirtualFile f) { return no("findDirectory"); }
  @Override public void reloadFromDisk(PsiFile f) { no("reloadFromDisk"); }
  @Override public void addPsiTreeChangeListener(PsiTreeChangeListener l) { no("addPsiTreeChangeListener"); }
  @Override public void addPsiTreeChangeListener(PsiTreeChangeListener l, Disposable d) { no("addPsiTreeChangeListener"); }
  @Override public void addPsiTreeChangeListenerBackgroundable(PsiTreeChangeListener l, Disposable d) { no("addPsiTreeChangeListenerBackgroundable"); }
  @Override public void removePsiTreeChangeListener(PsiTreeChangeListener l) { no("removePsiTreeChangeListener"); }
  @Override public void startBatchFilesProcessingMode() { no("startBatchFilesProcessingMode"); }
  @Override public void finishBatchFilesProcessingMode() { no("finishBatchFilesProcessingMode"); }
  @Override public <T> T runInBatchFilesMode(Computable<T> c) { return no("runInBatchFilesMode"); }
  @Override public void dropResolveCaches() { no("dropResolveCaches"); }
  @Override public void dropPsiCaches() { no("dropPsiCaches"); }
  @Override public boolean isInProject(PsiElement e) { return no("isInProject"); }
  @Override public FileViewProvider findCachedViewProvider(VirtualFile f) { return no("findCachedViewProvider"); }
}
