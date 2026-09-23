//W015J041   JOB (670W0020200W015J041,W100),'RTN W015D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM TIME=5,FORMS=1800,LINECT=0                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
//*                                                                             
//W015P041 EXEC W015P041                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W015J041                                            
