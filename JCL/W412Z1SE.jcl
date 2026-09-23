//W412Z1SE JOB (640W4120100W412Z1SE,W100),'RTN W412D4 ',                        
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
//* PIE KVITTO FÖR SW-ORDRAR                                                    
//*************  VCAS                                                           
//VCOM     EXEC W016P022,VCOM=W412Z1SE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D4.W41228(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412Z1SE                                         
