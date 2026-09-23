//W015J031   JOB (670W0020200W015J031,W100),'RTN W015D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM TIME=5,FORMS=1800,LINECT=0                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
//*                                                                             
//W015P031 EXEC W015P031,CPU=2                                                  
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W015J031                                            
