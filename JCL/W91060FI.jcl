//W91060FI JOB (640W9100100W91060FI,W100),'RTN W910V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W91060  EXEC VXFER                                                            
//SYSIN   DD *                                                                  
  COPY DSN(WUT.W910V2.W91061(+0))                                               
  DEST(A729V2IN)                                                                
  TYPE(STD,ARTIKEL)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W91060OK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),SUM)                                      
  SNOTIFER(SUB,W.QASE.JCL(W91060ER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W91060).                                                               
//*                                                                             
//STXERR  IF   (W91060.VXFER.RC GT 0) THEN                                      
//RABEND  EXEC VRCABEND                                                         
//*                                                                             
//SOPABND EXEC WSOPEND,PROCESS=W91060FI                                         
//        ENDIF                                                                 
