//W51065FI JOB (640W5100100W51065FI,W100),'RTN W510D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WEMPTST EXEC WEMPTST,DSIN=WUT.W510D2.W51065(+0)                               
//  IF (WEMPTST.T.RC = 0) THEN    -- EJ TOM FIL -------------                   
//*                                                                             
//W51065  EXEC VXFER,COND=(0,LT,WEMPTST.T)                                      
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W510D2.W51065(+0))                                               
  DEST(A432) RAUTH                                                              
  TYPE(STD,A432PRES)                                                            
  SNOTIFOK(SUB,W.QASE.JCL(W51065OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(+0),TOT)                                     
  SNOTIFER(SUB,W.QASE.JCL(W51065ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(+0),ALL)                                     
  ACCOUNT(DEST)                                                                 
  XFERID(W51065).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOPERR  EXEC WSOP,COND=ONLY                                                   
ABEND W51065FI                                                                  
//*                                                                             
//    ELSE  -- TOM FIL -------------------------------------                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W51065FI                                         
//    ENDIF                                                                     
