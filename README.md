# Island Scrub Jay Habitat Data

## 📄 프로젝트 개요
Island Scrub Jay는 캘리포니아 산타 크루즈 섬에만 서식하는 종으로, 이들의 서식지 선호를 이해하고 예측하는 것은 생태계 관리 및 종 보전에 필수적이다. 본 프로젝트는 2008년 가을에 수집된 307개 조사 지점의 환경 및 공간 데이터를 기반으로, 변수 간의 관계를 설명할 수 있는 해석 가능한 모델을 구축하여 Island Scrub Jay 서식지의 특징을 분석하고 예측하는 것을 목표로 한다.

### 문제정의
- 섬 스크럽 제이의 서식지 특성과 환경적 요인은 무엇인가?


### 데이터셋 개요
이 데이터셋은 2008년 가을, 캘리포니아 산타 크루즈 섬에서 **Island Scrub Jay**의 서식지를 조사하여 수집된 것이다. **307개 조사 지점**에서 환경 및 공간적 특성을 측정하였으며, Island Scrub Jay의 서식지 선호 유형과 존재 여부를 예측하는 데 활용된다.

### 데이터셋 요약
- **전체 데이터셋 크기:** 5625개의 관측치
- **변수:**
  1. **isj:** Island Scrub Jay의 존재 여부 (`1`: 존재, `0`: 부재)
  2. **x:** 조사 지점의 X 좌표
  3. **y:** 조사 지점의 Y 좌표
  4. **elev:** 고도
  5. **forest:** forest 비율
  6. **chap:** chaparral 비율
- **결측치:**
  - NA가 포함된 관측치: **5322개**

---

## 🎯 분석 목표

### 목표 1: Scrub Jay의 선호 지형 이해
- **고도**, **forest 비율**, **chaparral 비율**과 같은 환경 변수를 활용해 Island Scrub Jay의 존재 여부(`isj`)와의 관계를 분석한다.

### 목표 2: Scrub Jay의 섬 내 존재 여부 예측
주어진 데이터셋을 활용하여 다음과 같은 예측모델을 구한다.:

  **오분류율이 가장 낮은 모델:**
   - 새롭게 정의한 변수 중 오분류율(Misclassification Rate)을 최소화하는 변수 조합을 선택한다.
   - 이 조합에 기존 변수를 추가하여 예측 모델을 생성한다.

---

## 🛠️ 분석 워크플로우

### 단계 1: 새로운 변수 생성
- 변수 간의 비선형 관계와 상호작용을 반영하기 위해 **다항식 변수(polynomial terms)** 와 **상호작용 변수(interaction terms)** 를 추가한다.

### 단계 2: 데이터 분할
- 데이터를 **학습(train, 70%)** 과 **테스트(test, 30%)** 데이터로 나눈다.

### 단계 3: 모델 선택
1. **오분류율 최소화 변수 조합**
   - 설명변수들 중에서 오분류율을 최소화하는 조합을 찾기 위해 for문을 사용한다.

### 단계 4: 가설 검정
- 반응 변수(`isj`)에 유의미한 영향을 미치는 변수를 식별하기 위해 가설 검정을 진행한다.

### 단계 5: 최종 모델과 시각화
- 오분류율이 가장 낮은 변수의 조합을 최종 모델로 선택한다.
- 선택된 모델의 결과를 시각화하고, 최종적으로 2484개의 데이터셋에 모델을 적용하여 예측을 진행한다.




