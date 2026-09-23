//W22163FI JOB (650W2210100W22163FI,W100),'RTN W221D5',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W22163  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(W221.W221D5.W22162(0))                                               
  DEST(AMOSWPAR)                                                                
  TYPE(STD,WPAR3201)                                                            
  SNOTIFOK(SUB,W.QASE.JCL(W22163OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W22163ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(650W2210100,W100)                                                     
  XFERID(W22163).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W22163FI                                                                  
