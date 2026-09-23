//WF10R1RE JOB (640WF100100WF10R1RE,W100),'RTN WF10R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=WF10R1,MAXRC=8                                        
//*                                                                             
//EMPTYTST EXEC WEMPTST,DSIN=WF10.WF10R1.WF1021(0)                              
//    IF (EMPTYTST.T.RC = 0) THEN                                               
//      EXEC WSOP,COMMAND='ACTIVATE WF10R2'                                     
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10R1RE                                         
