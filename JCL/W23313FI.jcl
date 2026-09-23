//W23313FI JOB (650W2330100W23313FI,W100),'RTN W233PV',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W23313  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W233PV.W23313(0))                                                
  DEST(A310) RAUTH                                                              
  TYPE(STD,A31002A)                                                             
  XFERID(W23313)                                                                
  SNOTIFOK(SUB,W.QASE.JCL(W23313OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W23313ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W23313FI                                                                  
