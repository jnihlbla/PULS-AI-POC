//W510Z3SE JOB (650W5100100W510Z3SE,W100),'RTN W510D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* VCOM ÖVERFÖRING TILL SYSTEM PROCAST (SPECIALVAGNAR)                         
//*************  PRISTRANSAR PÅ LEV 1441                                        
//VCOM     EXEC W016P022,VCOM=W510Z3SE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W510D3.W51034(+0),DISP=SHR                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W510Z3SE                                         
