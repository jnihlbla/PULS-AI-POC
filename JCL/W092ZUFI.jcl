//W092ZUFI JOB (650W0920100W092ZUFI,W100),'RTN W092D6',                         
//             USER=?,PASSWORD=?,                                               
//          CLASS=K                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*---------------------------------------------------------------------        
//W092ZUT EXEC WEMPTST,DSIN=WUT.W092D6.W092ZU(+0)                               
//W092ZU  EXEC VXFER,COND=(0,LT,W092ZUT.T)                                      
//SYSIN    DD  *                                                                
  COPY DSN(WUT.W092D6.W092ZU(+0))                                               
  DEST(P660) RAUTH                                                              
  TYPE(STD,P66003A)                                                             
  SNOTIFOK(SUB,W.QASE.JCL(W092ZUOK),                                            
          ,ULOG,F1XFVC.PROD.TOTLOG(0),TOT)                                      
  SNOTIFER(SUB,W.QASE.JCL(W092ZUER),                                            
          ,ULOG,F1XFVC.PROD.ERRLOG(0),ALL)                                      
  ACCOUNT(DEST)                                                                 
  XFERID(W092ZU).                                                               
//*                                                                             
//RABEND  EXEC VRCABEND,COND=((0,LT,W092ZUT.T),(4,GE,W092ZU.VXFER))             
//*                                                                             
//SOPAB   EXEC WSOPEND,PROCESS=W092ZUFI,                                        
//             COND.SOPEND=(0,EQ,W092ZUT.T)                                     
