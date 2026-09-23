//W159Z1SE JOB (640W1590100W159Z1SE,W100),'RTN W159D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  SKICKAR DAGLIG ARTIKELINFO TILL NEVIS (MQ VIA VCOM)                        
//*  SENDER TAG = VCCIS040A CRL                                                 
//*  INITIATOR  = 41                                                            
//*                                                                             
//*  EXPEDITER  = VMQPD703                                                      
//*  NET        = VCCSEG1                                                       
//*  NODE       = VCOMVC01 / GBW02014                                           
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=WUT.W159D1.W15902(+0)                               
//VCOM    EXEC W016P022,VCOM=W159Z1SE,COND=(0,LT,TOM.T)                         
//W01622.W016ZZD1 DD DSN=WUT.W159D1.W15902(+0),DISP=SHR                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W159Z1SE                                         
