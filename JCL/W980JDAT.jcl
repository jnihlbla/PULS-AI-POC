//W980JDAT JOB (650W0090100W980JDAT,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//DATCHG  EXEC WSOPDAT                                                          
//*                                                                             
//VRCABE EXEC  VRCABEND,COND=(8,GT)                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JDAT                                         
