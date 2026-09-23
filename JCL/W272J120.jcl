//W272J120 JOB (640W2720100W272J120,W100),'RTN W271V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P020,                                                        
//             INDUT=W271.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J120                                         
