//W475TRFI JOB (650W4750100W475TRFI,W100),'RTN W475V1',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*---------------------------------------------------------------------        
//W475TR  EXEC VXFER                                                            
   COPY DSN(WUT.W475V1.W475TR(0))                                               
   DEST(A743V2IN)                                                               
   TYPE(STD,A743PAAR) RAUTH                                                     
   XFERID(W475TR)                                                               
   SNOTIFOK(SUB,W.QASE.JCL(W475TROK),                                           
           ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                     
   SNOTIFER(SUB,W.QASE.JCL(W475TRER),                                           
           ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL).                                    
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W475TRFI                                                                  
