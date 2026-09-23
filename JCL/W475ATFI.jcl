//W475ATFI JOB (640W4750100W475ATFI,W100),'RTN W475D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W475ATT EXEC WEMPTST,DSIN=WUT.W475D2.W475AT(+0)                               
// IF (W475ATT.T.RC = 0) THEN                                                   
//W475AT  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
   COPY DSN(WUT.W475D2.W475AT(+0))                                              
   DEST(VEDIVCAS)                                                               
   TYPE(STD,BJOHNJA)                                                            
   RAUTH (UDVEDI1,VDNAEDI)                                                      
   XFERID(W475AT)                                                               
   SNOTIFOK(SUB,W.QASE.JCL(W475ATOK),                                           
            ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                    
   SNOTIFER(SUB,W.QASE.JCL(W475ATER),                                           
            ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL).                                   
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(4,GE,W475AT.VXFER)                                
//SOPABE  EXEC WSOP,COMMAND='ABEND W475ATFI',                                   
//             COND=ONLY                                                        
//  ELSE                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W475ATFI                                         
//  ENDIF                                                                       
