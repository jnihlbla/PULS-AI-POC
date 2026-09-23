//W221J042 JOB (670W2210100W221J042,W100),'RTN W114D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
//*                                                                             
//W221     EXEC W221P042                                                        
//*                                                                             
//W22142.SYSUDUMP DD SYSOUT=*                                                   
//W22142.CEEDUMP  DD SYSOUT=*                                                   
//W22142.IDIREPRT DD SYSOUT=(A,,SYST)                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J042                                         
