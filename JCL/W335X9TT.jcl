//W335T1TT JOB (510W3350100),'NDC RTN W335X9',                                  
//             MSGCLASS=H,MSGLEVEL=(1,1),                                       
//             CLASS=N,TIME=1,NOTIFY=NONE                                       
/*JOBPARM ROOM=PVV2,LINES=15,CARDS=0,FORMS=STD,LINECT=00                        
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.XDEV.PROCLIB,W.QASE.PROCLIB)                            
//ENV  INCLUDE MEMBER=ENVXDEV                                                   
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,NAME='BOMANYSTEMXTRA',                    
//  ROOM=PVV2,DEPT='57051',ADDRESS=('VOLVO CAR AFTERSALES',                     
//  'TORSLANDA','405 08 GOTHENBURG')                                            
//*                                                                             
//*                                                                             
//SOP     EXEC WSOP2                                                            
//IN      DD   *                                                                
  ACTIVATE W335X9 SYMBOLS                                                       
    VCOM(W335X9TT)                                                              
  END-ORDER                                                                     
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W335X9'                                      
//ABE     ENDIF                                                                 
