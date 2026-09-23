//W475TOFI JOB (640W4750100W475TOFI,W100),'RTN W475D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W475TOT EXEC WEMPTST,DSIN=WUT.W475D2.W475TO(+0)                               
// IF (W475TOT.T.RC = 0) THEN                                                   
//W475TO  EXEC VXFER                                                            
//SYSIN    DD  *                                                                
   COPY DSN(WUT.W475D2.W475TO(+0))                                              
   DEST(VEDIVCAS)                                                               
   TYPE(STD,BSCHENK)                                                            
   RAUTH (UDVEDI1,VDNAEDI)                                                      
   XFERID(W475TO)                                                               
   SNOTIFOK(SUB,W.QASE.JCL(W475TOOK),                                           
            ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                    
   SNOTIFER(SUB,W.QASE.JCL(W475TOER),                                           
            ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL).                                   
//*                                                                             
//RABEND  EXEC VRCABEND,COND=(4,GE,W475TO.VXFER)                                
//SOPABE  EXEC WSOP,COMMAND='ABEND W475TOFI',                                   
//             COND=ONLY                                                        
//  ELSE                                                                        
//SOPEND  EXEC WSOPEND,PROCESS=W475TOFI                                         
//  ENDIF                                                                       
