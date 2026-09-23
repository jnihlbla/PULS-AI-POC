//W479J058 JOB (640W4790100W479J058,W100),'RTN W479V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P058                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J058                                         
