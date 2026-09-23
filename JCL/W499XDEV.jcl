//W499XDEV JOB (640W0090100W499XDEV,W100),'RTN W499XX',                         
//        CLASS=L,USER=?,PASSWORD=?                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*THIS JOB MAKES POSSIBLE TO SUBMIT FIX PROGRAMS FROM XDEV ENV!                
//*                                                                             
//SUB1   EXEC WFSUBMIT,JCLLIB=W.XDEV.JCL,SUBMIT=W499J003                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W499XDEV                                         
