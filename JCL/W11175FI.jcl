//W11175FI JOB (640W9100100W11175FI,W100),'RTN W111V5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W11175  EXEC VXFER                                                            
//SYSIN   DD *                                                                  
  COPY DSN(WUT.W111V5.W11175(+0))                                               
  DEST(A729V2IN)                                                                
  TYPE(STD,ARTIKEL)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W11175OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),SUM)                                      
  SNOTIFER(SUB,W.QASE.JCL(W11175ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W11175).                                                               
//*                                                                             
//STXERR  IF   (W11175.VXFER.RC GT 0) THEN                                      
//RABEND  EXEC VRCABEND                                                         
//*                                                                             
//SOPABND EXEC WSOPEND,PROCESS=W11175FI                                         
//        ENDIF                                                                 
