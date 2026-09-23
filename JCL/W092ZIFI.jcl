//W092ZIFI JOB (650W0920100W092ZIFI,W100),'RTN W092D6',                         
//             USER=?,PASSWORD=?,                                               
//          CLASS=K                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*---------------------------------------------------------------------        
//W092ZIT EXEC WEMPTST,DSIN=WUT.W092D6.W092ZI(+0)                               
//W092ZI  EXEC VXFER,COND=(0,LT,W092ZIT.T)                                      
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W092D6.W092ZI(+0))                                               
  DEST(A729V2IN) RAUTH                                                          
  TYPE(STD,PARTS)                                                               
  SNOTIFOK(SUB,W.QASE.JCL(W092ZIOK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W092ZIER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  XFERID(W092ZI).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=((0,LT,W092ZIT.T),(4,GE,W092ZI.VXFER))             
//*                                                                             
//SOPAB   EXEC WSOPEND,PROCESS=W092ZIFI,                                        
//             COND.SOPEND=(0,EQ,W092ZIT.T)                                     
