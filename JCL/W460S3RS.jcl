//W460S3RS JOB (650W4600100W460S3RS,W100),'RTN W460S3',                         
//         CLASS=K,USER=?,PASSWORD=?                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM=&VCOM                                                                  
//* COUNTRYX2=&COUNTRYX2                                                        
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD *                                                                  
 %WRTNIN2 W460.W460X1&COUNTRYX2..W46001 W460.W460S3.W46001                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W460S3RS                                         
