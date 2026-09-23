//W47584FI JOB (640W4750100W47584FI,W100),'RTN W475D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W47584T EXEC WEMPTST,DSIN=WUT.W475D2.W47584(+0)                               
// IF (W47584T.T.RC = 0) THEN                                                   
//W47584  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
   COPY DSN(WUT.W475D2.W47584(+0))                                              
   DEST(VEDIVCAS)                                                               
   TYPE(STD,BBURGHA)                                                            
   RAUTH (UDVEDI1,VDNAEDI)                                                      
   XFERID(W47584)                                                               
   SNOTIFOK(SUB,W.QASE.JCL(W47584OK),                                           
            ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                    
   SNOTIFER(SUB,W.QASE.JCL(W47584ER),                                           
            ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL).                                   
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(4,GE,W47584.VXFER)                                
//SOPABE  EXEC WSOP,COMMAND='ABEND W47584FI',                                   
//             COND=ONLY                                                        
//  ELSE                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W47584FI                                         
//  ENDIF                                                                       
