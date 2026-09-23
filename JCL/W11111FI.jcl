//W11111FI JOB (640W1110100W11111FI,W100),'RTN W111V1',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W11111     EXEC VXFER                                                         
//SYSIN DD *                                                                    
COPY DSN(WUT.W111V1.W11111(+0))                                                 
DEST(R111V2) TYPE(STD,PROD)                                                     
XFERID(W11111)                                                                  
SNOTIFOK(SUB,W.QASE.JCL(W11111OK),                                              
        ,ULOG,F1XFVC.PROD.TOTLOG(+0),TOT)                                       
SNOTIFER(SUB,W.QASE.JCL(W11111ER),                                              
        ,ULOG,F1XFVC.PROD.ERRLOG(+0),ALL).                                      
//*                                                                             
//VRCABE  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W11111FI                                                                  
