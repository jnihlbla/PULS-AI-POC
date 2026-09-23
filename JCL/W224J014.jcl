//W224J014 JOB (670W2240100W224J014,W100),'RTN W224V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM ROOM=ARHK,FORMS=1800,LINECT=0                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
//*                                                                             
//W224P014 EXEC W224P014                                                        
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J014                                         
