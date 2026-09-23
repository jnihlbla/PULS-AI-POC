//W115JMTS   JOB (650W1150100W115JMTS,W100),'RTN W115D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
//TST21  EXEC WEMPTST,DSIN=W115.W115D1.W11521(+0)                               
//ACT    EXEC WSOP,COND=(0,LT,TST21.T),COMMAND='ACTIVATE W115JMSD'              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W115JMTS                                         
