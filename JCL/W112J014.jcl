//W112J014 JOB (640W1120100W112J014,W100),'RTN W112PV',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W112    EXEC W112P014                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W112J014                                         
