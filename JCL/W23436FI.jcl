//W23436FI JOB (640W2340100W23436FI,W100),'RTN W234V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W23436  EXEC VXFER                                                            
//SYSIN   DD   *                                                                
  COPY DSN(WUT.W234V1.W23436(+0))                                               
  DEST(R412D1)                                                                  
  TYPE(STD,PROD)                                                                
  SNOTIFOK(SUB,W.QASE.JCL(W23436OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),SUM)                                      
  SNOTIFER(SUB,W.QASE.JCL(W23436ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W23436).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(4,GE,W23436.VXFER)                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W23436FI,                                        
//             COND.SOPEND=(4,GE,W23436.VXFER)                                  
