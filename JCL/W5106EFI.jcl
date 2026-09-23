//W5106EFI JOB (640W5100100W5106EFI,W100),'RTN W510D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WEMPTST EXEC WEMPTST,DSIN=WUT.W510D2.W5106E(+0)                               
//  IF (WEMPTST.T.RC = 0) THEN    -- EJ TOM FIL -------------                   
//*                                                                             
//W5106E  EXEC VXFER,COND=(0,LT,WEMPTST.T)                                      
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W510D2.W5106E(+0))                                               
  DEST(A432) RAUTH                                                              
  TYPE(STD,A432PRES)                                                            
  SNOTIFOK(SUB,W.QASE.JCL(W5106EOK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(+0),TOT)                                     
  SNOTIFER(SUB,W.QASE.JCL(W5106EER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(+0),ALL)                                     
  ACCOUNT(DEST)                                                                 
  XFERID(W5106E).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOPERR  EXEC WSOP,COND=ONLY                                                   
ABEND W5106EFI                                                                  
//*                                                                             
//    ELSE  -- TOM FIL -------------------------------------                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W5106EFI                                         
//    ENDIF                                                                     
