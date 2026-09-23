//W91050FI JOB (650W9100100W91050FI,W100),'RTN W91050',                         
//         CLASS=K,USER=?,PASSWORD=?                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W91050  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W91050.W91050(0))                                                
  DEST(P840)                                                                    
  TYPE(STD,P84002D)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W91050OK),,                                           
           ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91050ER),,                                           
           ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W91050)                                                                
  ACCOUNT(DEST).                                                                
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W91050FI                                                                  
