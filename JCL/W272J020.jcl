//W272J020 JOB (640W2720100W272J020,W100),'RTN W271D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P020,                                                        
//             INDUT=W271.W271D1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J020                                         
