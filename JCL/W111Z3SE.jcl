//W111Z3SE JOB (640W1590100W111Z3SE,W100),'RTN W111D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  SKICKAR DAGLIG ERSÄTTNINGSINFO TILL NEVIS (MQ VIA VCOM)                    
//*  SENDER TAG = VCCIS041A CRL                                                 
//*  INITIATOR  = 41                                                            
//*                                                                             
//*  EXPEDITER  = VMQPD701                                                      
//*  NET        = VCCSEG1                                                       
//*  NODE       = VCOMVC01 / GBW02014                                           
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W111D1.W1117N(+0)                               
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W111Z3SE,COND=(0,LT,TOM.T)                         
//W01622.W016ZZD1 DD DSN=WUT.W111D1.W1117N(+0),DISP=SHR                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111Z3SE                                         
