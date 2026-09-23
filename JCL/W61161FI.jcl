//W61161FI JOB (650W6110100W61161FI,W100),'RTN W611S5',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*---------------------------------------------------------------------        
//W61161  EXEC VXFER                                                            
  COPY DSN(WUT.W611S5.W61161(0))                                                
  DEST(NJEILINK)                                                                
  TODSN(W611V2.PDP.W61161(+1))                                                  
  DISP(CATLG) MGMTCLAS(DEL2BKPC)                                                
  SNOTIFOK(SUB,W.QASE.JCL(W61161OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W61161ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W61161).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W61161FI                                                                  
