//W020V1RE JOB (650W0510100W020V1RE,W100),'RTN W020V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W020V1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W020V1RE                                         
//*                                                                             
//SOPINST EXEC WSOP                                                             
IF-CALENDAR BATCH06                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(VECKOBATCH KLAR)                                                        
  END-ACTIVATE                                                                  
END-IF                                                                          
IF-CALENDAR BATCH01                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(VECKOBATCH KLAR)                                                        
  END-ACTIVATE                                                                  
END-IF                                                                          
