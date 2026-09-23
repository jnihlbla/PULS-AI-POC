//W91036FI JOB (540W9100100W91036FI,W100),'RTN W910B4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W91036  EXEC VXFER                                                            
  COPY DSN(WUT.W910B4.W91036(+0))                                               
  DEST(D886) RAUTH                                                              
  TYPE(STD,VCASREC)                                                             
  XFERID(W91036)                                                                
  SNOTIFOK(SUB,W.QASE.JCL(W91036OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91036ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W91036FI                                                                  
