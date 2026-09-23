//W475UTFI JOB (650W4750100W475UTFI,W100),'RTN W475S4',                         
//          CLASS=K,USER=?,PASSWORD=?                                           
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WEMPTST EXEC WEMPTST,DSIN=WUT.W475S4.W475UT(+0)                               
//*                                                                             
//W475UTD EXEC PGM=IEFBR14,COND=(0,EQ,WEMPTST.T)                                
//DD       DD  DSN=WUT.W475S4.W475UT(+0),DISP=(OLD,DELETE)                      
//*                                                                             
//W475UT  EXEC VXFER,COND=(0,LT,WEMPTST.T)                                      
//SYSIN DD *                                                                    
COPY DSN(WUT.W475S4.W475UT(+0))                                                 
DEST(RVSPARTS) RAUTH                                                            
TYPE(STD,TDS1)                                                                  
XFERID(W475S4)                                                                  
SNOTIFOK(SUB,W.QASE.JCL(W475UTOK),                                              
        ,ULOG,F1XFVC.PROD.TOTLOG(+0),TOT)                                       
SNOTIFER(SUB,W.QASE.JCL(W475UTER),                                              
        ,ULOG,F1XFVC.PROD.ERRLOG(+0),ALL).                                      
//*                                                                             
//WABEND  EXEC VRCABEND,COND=(7,GE)                                             
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W475UTFI                                                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W475UTFI,                                        
//             COND.SOPEND=(0,EQ,WEMPTST.T)                                     
