//W015J092 JOB (51091927000),'LARS Q3 Q2    ',                                  
//             MSGCLASS=H,MSGLEVEL=(1,1),                                       
//             CLASS=K,NOTIFY=PC13343                                           
/*JOBPARM ROOM=HC2N,TIME=0,LINES=9,CARDS=0,FORMS=STD,LINECT=00                  
//PROC  JCLLIB ORDER=(W.IGRT.PROCLIB,W.QASE.PROCLIB)                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015     EXEC W015P092                                                        
//*                                                                             
