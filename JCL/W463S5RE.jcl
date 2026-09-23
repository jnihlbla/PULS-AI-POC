//W463S5RE JOB (650W4630100W463S5RE,W100),'RTN W463S5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=010                                                
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463S5.W46336(+0)                              
//*                                                                             
//    IF (EMPTYT.T.RC = 0) THEN                                                 
//      EXEC WSOP                                                               
        ORDER W463S3                                                            
//    ELSE                                                                      
//DEL   EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W463.W463S5.W46336(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W463S5,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S5RE                                         
