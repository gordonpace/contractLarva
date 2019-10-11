pragma solidity ^0.5.11;
contract LARVA_MultiOwners {
  modifier LARVA_DEA_2_handle_after_removeOwner__parameters__address (address _address) {
    if ((LARVA_STATE_2 == 0)) {
      LARVA_STATE_2 = 0;
      votes[_address][msg.sender] = true;
    }
    _;
  }
  modifier LARVA_DEA_2_handle_before_removeOwner__parameters__address (address _address) {
    if ((LARVA_STATE_2 == 0) && (votes[_address][msg.sender])) {
      LARVA_STATE_2 = 1;
      LARVA_reparation();
    }
    _;
  }
  modifier LARVA_DEA_1_handle_after_proposeTransaction__parameters__to__val (address payable _to, uint _val) {
    if ((LARVA_STATE_1 == 0)) {
      LARVA_STATE_1 = 0;
      idRequestedBy[--id] = msg.sender;
    }
    _;
  }
  modifier LARVA_DEA_1_handle_before_transfer__parameters__id (uint _id) {
    if ((LARVA_STATE_1 == 0) && (!owners[idRequestedBy[id]])) {
      LARVA_STATE_1 = 1;
      LARVA_reparation();
    }
    _;
  }
  int8 LARVA_STATE_1 = 0;
  int8 LARVA_STATE_2 = 0;
  function LARVA_reparation () private {
    revert();
  }
  mapping (uint => address) idRequestedBy;
  mapping (address => mapping (address => bool)) votes;
  modifier LARVA_Constructor {
    {
    }
    LARVA_Status = LARVA_STATUS.RUNNING;
    _;
  }
  enum LARVA_STATUS {NOT_STARTED, READY, RUNNING, STOPPED}
  function LARVA_EnableContract () LARVA_ContractIsEnabled private {
    LARVA_Status = (LARVA_Status == LARVA_STATUS.NOT_STARTED)?LARVA_STATUS.READY:LARVA_STATUS.RUNNING;
  }
  function LARVA_DisableContract () LARVA_ContractIsEnabled private {
    LARVA_Status = (LARVA_Status == LARVA_STATUS.READY)?LARVA_STATUS.NOT_STARTED:LARVA_STATUS.STOPPED;
  }
  LARVA_STATUS private LARVA_Status = LARVA_STATUS.NOT_STARTED;
  modifier LARVA_ContractIsEnabled {
    require(LARVA_Status == LARVA_STATUS.RUNNING);
    _;
  }
  mapping (address => bool) public owners;
  address[] ownerList;
  uint ownerCount;
  mapping (uint => mapping (address => bool)) actionSignOffs;
  constructor (address[] memory _owners) LARVA_Constructor public {
    for (uint i = 0; i < _owners.length; i++) {
      owners[_owners[i]] = true;
    }
    ownerList = _owners;
    ownerCount = ownerList.length;
  }
  modifier anyOwner {
    require(owners[msg.sender]);
    _;
  }
  modifier allOwners (uint _id) {
    require(owners[msg.sender]);
    actionSignOffs[_id][msg.sender] = true;
    for (uint i = 0; i < ownerList.length; i++) {
      if (ownerList[i] != address(0) && !actionSignOffs[_id][ownerList[i]]) {
        return;
      }
    }
    _;
  }
  mapping (address => mapping (address => bool)) votesToRemove;
  mapping (address => address[]) votesToRemoveKeys;
  function removeOwner (address _address) LARVA_DEA_2_handle_after_removeOwner__parameters__address(_address) LARVA_DEA_2_handle_before_removeOwner__parameters__address(_address) LARVA_ContractIsEnabled anyOwner public {
    votesToRemove[_address][msg.sender] = true;
    votesToRemoveKeys[_address].push(msg.sender);
    uint countInFavour = 0;
    uint totalCount = ownerCount;
    for (uint i = 0; i < totalCount; i++) {
      if (votesToRemove[_address][votesToRemoveKeys[_address][i]]) {
        countInFavour++;
      }
    }
    uint limit = 2 * totalCount / 3;
    if (countInFavour >= limit) {
      owners[_address] = false;
      (ownerCount)--;
      for (uint i = 0; i < ownerList.length; i++) {
        if (ownerList[i] != address(0) && ownerList[i] == _address) {
          ownerList[i] = address(0);
        }
      }
    }
  }
  mapping (uint => address payable) idTo;
  mapping (uint => uint) idVal;
  uint id;
  function proposeTransaction (address payable _to, uint _val) LARVA_DEA_1_handle_after_proposeTransaction__parameters__to__val(_to, _val) LARVA_ContractIsEnabled public anyOwner {
    idTo[id] = _to;
    idVal[id] = _val;
    id++;
  }
  function transfer (uint _id) LARVA_DEA_1_handle_before_transfer__parameters__id(_id) LARVA_ContractIsEnabled allOwners(_id) public {
    idTo[_id].transfer(idVal[_id]);
  }
  function () external payable {
  }

}
