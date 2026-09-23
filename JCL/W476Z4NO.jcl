//W476Z4NO JOB (670W4760100W476Z4NO,W100),'RTN W476D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* FAKTURAUPPG FÖR EDI ÖVERFÖRING                                              
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476D3.W47626(+0)                              
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W476.W476D3.W47626(+0),DISP=(OLD,DELETE)                         
//*                                                                             
//****LSE                                                                       
//****     EXEC W016P022,VCOM=W475Z1NO                                          
//****                                                                          
//****22.W016ZZD1 DD DSN=W476.W476D3.W47626(+0),DISP=SHR                        
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476Z4NO                                         
