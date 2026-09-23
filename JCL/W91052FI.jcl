//W91052FI JOB (650W9100100W91052FI,W100),'RTN W91052',                         
//         CLASS=K,USER=?,PASSWORD=?                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W91052  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W91052.W91052(0))                                                
  DEST(A323A) RAUTH                                                             
  TYPE(STD)                                                                     
  SNOTIFOK(SUB,W.QASE.JCL(W91052OK),,                                           
           ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91052ER),,                                           
           ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W91052)                                                                
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W91052FI                                                                  
