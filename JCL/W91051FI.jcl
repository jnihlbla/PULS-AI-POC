//W91051FI JOB (650W9100100W91051FI,W100),'RTN W91051',                         
//         CLASS=K,USER=?,PASSWORD=?                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W91051  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W91051.W91051(0))                                                
  DEST(T001) RAUTH                                                              
  TYPE(STD,T00101A)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W91051OK),,                                           
           ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91051ER),,                                           
           ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W91051)                                                                
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W91051FI                                                                  
