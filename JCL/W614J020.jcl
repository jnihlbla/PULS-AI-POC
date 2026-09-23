//W614J020 JOB (640W6140100W614J020,W100),'RTN W614S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W614    EXEC W614P020                                                         
//W61420.W61420D1 DD *                                                          
&URVAL                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W614J020                                         
