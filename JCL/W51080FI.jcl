//W51080FI JOB (650W5100100W51080FI,W100),'RTN W510D4',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W51080  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W510D4.W51080(0))                                                
  DEST(A432) RAUTH                                                              
  TYPE(STD,A43201R)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W51080OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W51080ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST)                                                                 
  XFERID(W51080).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W51080FI                                                                  
