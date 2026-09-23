//W115J014   JOB (670W1150100W115J014,W100),'RTN W115D2',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
//W115P014 EXEC W115P014                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W115J014                                            
