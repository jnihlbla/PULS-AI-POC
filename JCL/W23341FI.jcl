//W23341FI JOB (650W2330100W23341FI,W100),'RTN W233PV',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W23341  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W233PV.W23341(0))                                                
  DEST(A310) RAUTH                                                              
  TYPE(STD,A31002B)                                                             
  XFERID(W23341)                                                                
  SNOTIFOK(SUB,W.QASE.JCL(W23341OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W23341ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W23341FI                                                                  
