//W47301FI JOB (540W4730100W47301FI,W100),'RTN W473S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W47301  EXEC VXFER                                                            
  COPY DSN(WUT.W473S1.W47301(+0))                                               
  DEST(D886) RAUTH                                                              
  TYPE(STD,PVPACK)                                                              
  XFERID(W47301)                                                                
  SNOTIFOK(SUB,W.QASE.JCL(W47301OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W47301ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W47301FI                                                                  
