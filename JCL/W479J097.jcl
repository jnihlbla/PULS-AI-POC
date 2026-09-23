//W479J097 JOB (640W4790100W479J097,W100),'RTN W479D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=25                                          
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P097                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J097                                         
