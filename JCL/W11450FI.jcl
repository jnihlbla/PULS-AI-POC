//W11450FI JOB (650W1140100W11450FI,W100),'RTN W114S2',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W11450     EXEC VXFER                                                         
//SYSIN DD *                                                                    
COPY DSN(WUT.W114S2.W11450(+0))                                                 
DEST(A32371) RAUTH                                                              
TYPE(STD,PROD)                                                                  
XFERID(W11450)                                                                  
SNOTIFOK(SUB,W.QASE.JCL(W11450OK),                                              
        ,ULOG,F1XFVC.PROD.TOTLOG(+0),TOT)                                       
SNOTIFER(SUB,W.QASE.JCL(W11450ER),                                              
        ,ULOG,F1XFVC.PROD.ERRLOG(+0),ALL).                                      
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W11450FI                                                                  
