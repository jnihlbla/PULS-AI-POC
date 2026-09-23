//W91007FI JOB (650W9100100W91007FI,W100),'RTN WWUTVV',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*---------------------------------------------------------------------        
//W91007  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
  COPY DSN(WUT.WWUTVV.W91007(0))                                                
  DEST(A470) RAUTH                                                              
  TYPE(STD,A47006P)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W91007OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91007ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST)                                                                 
  XFERID(W91007).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W91007FI                                                                  
