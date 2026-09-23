//W97003FI JOB (540W0000100W97003FI,W100),'RTN W970V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*ROUTE XEQ NJERS                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//XMIT    EXEC VXFER                                                            
//SYSIN DD DATA,DLM='??'                                                        
 COPY DSN(V760V2.USERS(0))                                                      
      TODSN(W970.W970B1.W97003(+1))                                             
      DEST(NJEV1)                                                               
      MGMTCLAS(BACKUPC)                                                         
      XFERID(W97003)                                                            
      SNOTIFER(SUB,W.PROD.JCL(W97003ER),                                        
              ,ULOG,F1XFV1.PROD.ERRLOG(+0),ALL)                                 
      SNOTIFOK(SUB,W.PROD.JCL(W97003OK),                                        
              ,ULOG,F1XFV1.PROD.TOTLOG(+0),TOT).                                
//*                                                                             
//WABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W97003FI                                                                  
