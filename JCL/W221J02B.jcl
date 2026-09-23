//W221J02B JOB (670W2210100W221J02B,W100),'RTN W221S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
//*                                                                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W221     EXEC W221P02B                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J02B                                         
