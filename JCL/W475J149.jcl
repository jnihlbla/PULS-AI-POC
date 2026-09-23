//W475J149 JOB (640W4750100W475J149,W100),'RTN W475V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W475    EXEC W475P149                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W475J149                                         
