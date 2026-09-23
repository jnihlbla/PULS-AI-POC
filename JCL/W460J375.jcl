//W460J375 JOB (670W4600100W460J375,W100),'RTN W460S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W460    EXEC W460P375                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460J375                                         
